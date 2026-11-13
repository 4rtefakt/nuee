# Écrit un script R autonome qui refait un modèle de Nuée avec R seul, sans Nuée ni autre paquet :
# du code écrit pour ce modèle seulement, à lire de haut en bas. Il contient ses données, ses
# termes X et Y (transformations, prétraitement des spectres, mise à l'échelle), l'algorithme
# NIPALS de son type (ACP, PLS ou OPLS ; les valeurs manquantes n'y sont traitées que si les
# données en ont), sa validation croisée, puis ses résultats : R2 et Q2, scores et loadings, T2
# de Hotelling et DModX avec leurs limites ; pour une PLS (PLS-DA, OPLS) les VIP, les
# coefficients, la CV-ANOVA et le test de permutation (un appel en commentaire) ; le jeu de
# test ; des graphiques en R de base. Une régression (regression.R) a son propre code : ses termes
# et sa réponse comme le moteur les fait, l'ajustement de lm, nlme::lme ou mgcv::gam (ces deux
# paquets sont livrés avec R), ses coefficients, sa vraisemblance et son AIC, sa validation
# croisée par les mêmes groupes, son jeu de test, les résidus quantiles d'un modèle de comptages,
# de présence-absence ou de valeurs positives (les mêmes tirages), et sa sélection par l'AIC quand
# elle a été faite (selection_aic). D'après l'extension du chemin : un script (.R), ou
# un rapport R Markdown (.Rmd) ou Quarto (.qmd) qui y ajoute le texte des méthodes (methodes.R) et
# les références ; il se compile avec knitr (bouton Knit de RStudio, ou quarto render).
#   donnees : "script" (les données dans le script) ou "csv" (un fichier à côté du script) ;
#   renv, rdata, rproj : écrire aussi renv.lock (versions de R et des paquets), les données et
#   le modèle ajusté en .RData, un projet RStudio (.Rproj), à côté du script ;
#   langue : "fr" ou "en", celle des commentaires, des noms et des messages du script.
# Avant d'écrire, le code du script est exécuté ici, sans le moteur, et ses résultats comparés à
# ceux de `m`, le modèle de l'application : un écart est renvoyé dans `ecart`. (`dossier_moteur`
# ne sert plus : le script ne recopie plus le moteur ; l'argument reste pour les appels.)
ecrire_script <- function(projet, d, chemin_script, dossier_moteur, m = NULL, donnees = c("script", "csv"),
                          renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  donnees <- match.arg(donnees)
  langue <- if (identical(langue, "en")) "en" else "fr"
  if (est_autre_analyse(d)) return(ecrire_script_analyse(projet, d, chemin_script, m, donnees, renv, rdata, rproj, version, langue))  # analyses.R
  format <- c(r = "R", rmd = "Rmd", qmd = "qmd")[[tolower(tools::file_ext(chemin_script))]]
  tab <- tableau_script(projet, ids_script(list(d), projet))
  nom_projet <- as.character(projet$source %||% "projet")
  m <- m %||% ajuster(projet, d)
  pl <- plan_modele(projet, d, m, tab)
  b <- besoins_script(list(pl), seance = FALSE)
  fonctions <- code_fonctions(b, list(pl), langue)
  attacher_mgcv(b)
  e <- environnement_script(tab, fonctions, langue)
  x <- if (isTRUE(pl$regression)) code_une_regression(pl, m, b, e, langue) else code_un_modele(pl, m, b, e, langue)

  fichiers <- fichiers_script(chemin_script, donnees, rdata, renv, rproj, langue)
  code_donnees <- donnees_script(tab, fichiers, langue)
  lignes <- if (format == "R") {
    c(if (isTRUE(pl$regression)) entete_regression(pl, nom_projet, version, fichiers, langue) else entete_script(pl, nom_projet, version, fichiers, langue), "",
      titre_partie(1, langue), fonctions,
      titre_partie(2, langue), code_donnees, "",
      titre_partie(3, langue), x$calcul, "",
      titre_partie(4, langue), x$sortie, "", texte_graphiques(langue), unlist(lapply(x$graphiques, function(g) c(g$code, ""))))
  } else {
    lignes_rapport(format, pl, langue, texte_methodes(projet, d, m, langue, version), fonctions, code_donnees, x)
  }
  ecrire_fichiers_script(lignes, fichiers, e, c(nom_script("donnees", langue), x$objets), c(if (format != "R") c("knitr", "rmarkdown"), paquets_script(b)))
  list(script = chemin_script, chemins = unname(fichiers), ecart = x$ecart)
}

# Les paquets des régressions (livrés avec R) : nlme (un modèle mixte, l'autocorrélation d'un
# GAMM), mgcv, MASS (les tirages des seuils d'une courbe : MASS::mvrnorm) ; pour renv.lock et
# l'en-tête du script.
paquets_script <- function(b) c(if (isTRUE(b$lmm) || isTRUE(b$gamm)) "nlme", if (isTRUE(b$mgcv)) "mgcv", if (isTRUE(b$courbes)) "MASS")

# Les paquets d'un script, pour son en-tête : « R et ses paquets nlme, mgcv et MASS (livrés avec
# R) », ou « R seul » ; `versions` : « R 4.6.2 et mgcv 1.9-3 ».
texte_paquets <- function(paquets, langue, versions = FALSE) {
  tx <- function(fr, en) if (langue == "en") en else fr
  if (versions) {
    return(paste0("R ", paste(R.version$major, R.version$minor, sep = "."),
                  paste(vapply(paquets, function(pq) sprintf(tx(" et %s %s", " and %s %s"), pq, utils::packageDescription(pq)$Version), ""), collapse = "")))
  }
  liste <- sub(", ([^,]*)$", tx(" et \\1", " and \\1"), paste(paquets, collapse = ", "))
  if (length(paquets) > 1) sprintf(tx("R et ses paquets %s (livrés avec R)", "R and its packages %s (they come with R)"), liste)
  else if (length(paquets)) sprintf(tx("R et son paquet %s (livré avec R)", "R and its package %s (it comes with R)"), liste)
  else tx("R seul", "R alone")
}

# Le script d'un modèle de mgcv l'attache (library(mgcv) : ses lois nb() et tw() appellent ses
# fonctions depuis l'environnement global). Il l'est ici avant que l'environnement où le script est
# vérifié ne soit fait (son parent est fixé à sa création), pour que le script y trouve mgcv.
attacher_mgcv <- function(b) {
  if (isTRUE(b$mgcv) && !"package:mgcv" %in% search()) suppressPackageStartupMessages(library("mgcv", character.only = TRUE))
}

# Toute la séance dans un script .R : les données une fois, puis chaque modèle (`definitions`,
# et `modeles` ceux de l'application, dans le même ordre), ses résultats gardés dans une liste
# à son nom (M1, M2...). Mêmes options qu'ecrire_script() ; `ecart` nomme les modèles qui ne
# redonnent pas le leur.
ecrire_script_seance <- function(projet, definitions, modeles, chemin_script, dossier_moteur, donnees = c("script", "csv"),
                                 renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  donnees <- match.arg(donnees)
  langue <- if (identical(langue, "en")) "en" else "fr"
  tab <- tableau_script(projet, ids_script(definitions, projet))
  nom_projet <- as.character(projet$source %||% "projet")
  plans <- lapply(seq_along(definitions), function(k) plan_modele(projet, definitions[[k]], modeles[[k]], tab))
  b <- besoins_script(plans, seance = TRUE)
  fonctions <- code_fonctions(b, plans, langue)
  attacher_mgcv(b)
  e <- environnement_script(tab, fonctions, langue)
  blocs <- list()
  resultats <- list()
  ecarts <- character(0)
  for (k in seq_along(plans)) {
    blocs[[k]] <- if (isTRUE(plans[[k]]$regression)) code_une_regression(plans[[k]], modeles[[k]], b, e, langue, seance = k)
                  else code_un_modele(plans[[k]], modeles[[k]], b, e, langue, seance = k)
    resultats[[k]] <- mget(intersect(blocs[[k]]$objets, ls(e)), envir = e)
  }
  # chaque modèle sous son nom (M1...), sauf s'il est déjà pris par un nom du script
  code <- c(fonctions, unlist(lapply(blocs, function(x) c(x$calcul, x$sortie, unlist(lapply(x$graphiques, `[[`, "code"))))))
  pris <- c(symboles_code(code), nom_script("donnees", langue))
  noms <- make.unique(make.names(vapply(definitions, function(d) as.character(d$name %||% "M"), "")), sep = "_")
  noms[noms %in% pris] <- paste0(noms[noms %in% pris], if (langue == "en") "_model" else "_modele")
  tx <- function(fr, en) if (langue == "en") en else fr
  morceaux <- character(0)
  for (k in seq_along(plans)) {
    x <- blocs[[k]]
    if (!is.null(x$ecart)) ecarts <- c(ecarts, sprintf("%s : %s", noms[k], x$ecart))
    assign(noms[k], resultats[[k]], envir = e)
    pl <- plans[[k]]
    regression <- isTRUE(pl$regression)
    morceaux <- c(morceaux,
                  sprintf("# 3.%d %s : %s%s ----", k, noms[k], if (regression) type_regression(pl, langue) else type_script(pl, langue),
                          if (nzchar(pl$titre)) sprintf(" (%s)", texte_sur(pl$titre)) else ""),
                  x$calcul, "", x$sortie, if (pl$type %in% c("pls", "opls")) appel_permutations(pl, langue, noms[k]), "",
                  texte_graphiques(langue), unlist(lapply(x$graphiques, function(g) c(g$code, ""))),
                  commentaire(if (regression) tx(sprintf("Les résultats de %s, gardés sous son nom (%s$coefficients...).", noms[k], noms[k]),
                                                 sprintf("The results of %s, kept under its name (%s$coefficients...).", noms[k], noms[k]))
                              else tx(sprintf("Les résultats de %s, gardés sous son nom (%s$composantes...).", noms[k], noms[k]),
                                      sprintf("The results of %s, kept under its name (%s$components...).", noms[k], noms[k]))),
                  code_valeurs(sprintf("%s = %s", x$objets, x$objets), sprintf("%s <- list(", noms[k]), ")", ouvrir = ""), "")
  }
  fichiers <- fichiers_script(chemin_script, donnees, rdata, renv, rproj, langue)
  lignes <- c(entete_seance(plans, noms, nom_projet, version, fichiers, langue, paquets_script(b)), "",
              titre_partie(1, langue), fonctions,
              titre_partie(2, langue), donnees_script(tab, fichiers, langue), "",
              sprintf("# 3. %s ----", tx("Modèles et résultats", "Models and results")), "", morceaux)
  ecrire_fichiers_script(lignes, fichiers, e, c(nom_script("donnees", langue), noms), paquets_script(b))
  list(script = chemin_script, chemins = unname(fichiers), ecart = if (length(ecarts)) paste(ecarts, collapse = " ; "))
}

# L'environnement où le code du script est exécuté avant d'être écrit : ses données et ses
# fonctions, sans le moteur (son parent est celui de l'environnement global : R et ses paquets
# attachés, comme dans une session neuve).
environnement_script <- function(tab, fonctions, langue) {
  e <- new.env(parent = parent.env(globalenv()))
  assign(nom_script("donnees", langue), tab$donnees, envir = e)
  eval(parse(text = fonctions, encoding = "UTF-8"), envir = e)
  e
}

# Exécute du code du script dans `e` : NULL, ou le message de l'erreur ou de l'avertissement.
# `avertissements = FALSE` : un avertissement n'est pas un problème (une régression peut en donner,
# comme son ajustement dans le moteur ; le résultat est comparé ensuite).
evaluer_script <- function(lignes, e, avertissements = TRUE) {
  probleme <- NULL
  tryCatch(withCallingHandlers(eval(parse(text = lignes, encoding = "UTF-8"), envir = e),
                               warning = function(w) {
                                 if (avertissements) probleme <<- sprintf(traduire("avertissement : %s", "warning: %s"), conditionMessage(w))
                                 invokeRestart("muffleWarning")
                               }),
           error = function(err) probleme <<- sprintf(traduire("erreur : %s", "error: %s"), conditionMessage(err)))
  probleme
}

# Le code d'un modèle (`pl` : plan_modele()), exécuté dans `e` et comparé à `m`, celui de
# l'application. Le signe d'une composante étant arbitraire, le code est d'abord exécuté
# jusqu'à l'ajustement ; les composantes de signe opposé à celles de `m` sont ensuite
# retournées dans le script, pour qu'il donne les mêmes graphiques que Nuée.
code_un_modele <- function(pl, m, b, e, langue, seance = NULL) {
  code <- code_modele(pl, b, langue, seance)
  probleme <- evaluer_script(code$ajustement, e)
  r <- get0("m", envir = e, inherits = FALSE)
  if (is.null(probleme) && identical(dim(r$T), dim(m$T))) {
    signes <- vapply(seq_len(m$A), function(a) if (sum(m$T[, a] * r$T[, a], na.rm = TRUE) < 0) -1 else 1, 0)
    if (any(signes < 0)) code <- code_modele(pl, b, langue, seance, signes)
  }
  if (is.null(probleme)) probleme <- evaluer_script(c(code$orientation, code$statistiques), e)
  ecart <- if (!is.null(probleme)) probleme else ecart_script(pl, m, e, langue)
  list(calcul = c(code$ajustement, if (length(code$orientation)) "", code$orientation, "", code$statistiques),
       sortie = code$sortie, graphiques = code$graphiques, objets = code$objets, ecart = ecart)
}

# ---------------------------------------------------------------- le plan d'un modèle

# Ce que le script doit écrire pour le modèle `m` (de définition `d`) : ses observations (leurs
# rangs dans le tableau du script), ses termes X et Y (plan_bloc), le prétraitement des
# spectres, et ce que ses données demandent : valeurs manquantes, observations sans valeur X,
# termes constants, forme de DCrit...
plan_modele <- function(projet, d, m, tab) {
  if (est_regression(d)) return(plan_regression(projet, d, m, tab))
  pls <- identical(m$type, "PLS")
  opls <- isTRUE(m$opls)
  s <- m$statistiques
  A <- m$A
  N <- m$N
  K <- m$K
  test <- as.numeric(d$test %||% numeric(0))
  x <- plan_bloc(projet, d$x, m$x, tab, c(m$observations, test), "X")
  y <- if (pls) plan_bloc(projet, d$y, m$y, tab, c(m$observations, test), "Y", x$cles)
  bt <- if (length(test)) construire_bloc(projet, d$x, test, imposees = m$x, spectres = d$spectres)$X
  yt <- if (length(test) && pls) construire_bloc(projet, d$y, test, imposees = m$y)$X
  notes <- !is.na(m$T[, 1])
  n_y <- if (pls) colSums(m$MY == 1 & notes)
  dfm <- (N - 1 - A) * (K - A)
  presentes_test <- if (length(test)) rowSums(!is.na(bt))
  # les groupes de la validation croisée, s'ils ne sont pas à tour de rôle (un par valeur d'un
  # identifiant ou d'une variable : validation_croisee.R)
  groupes <- as.integer(m$vc$groupes)
  list(nom = as.character(d$name %||% ""), titre = as.character(d$title %||% ""),
       type = if (opls) "opls" else if (pls) "pls" else "acp", plsda = isTRUE(m$plsda),
       A = A, N = N, K = K, G = m$G, Ap = if (opls) m$Ap %||% 1 else 0, M = if (pls) ncol(m$Y) else 0,
       groupes_vc = if (length(groupes) && !identical(groupes, as.integer(groupes_validation(N, m$G)))) groupes,
       vc_par = m$vc_par %||% "", vc_fichier = identical(as.character(d$groupes_vc$by), "groups"),
       rangs = unname(tab$rangs[as.character(m$observations)]), rangs_test = unname(tab$rangs[as.character(test)]),
       condition = plan_condition(projet, d, tab),  # (condition.R)
       noms_tableau = as.character(tab$donnees[[1]]), colonnes = names(tab$donnees),
       x = x$termes, y = y$termes, spectres = plan_spectres(projet, d, x$termes),
       manquantes = any(m$MX == 0) || (pls && any(m$MY == 0)) || anyNA(bt),
       y_manquantes = pls && any(m$MY == 0), vides = any(!notes), ki_court = any(rowSums(m$MX) <= A),
       dmodx = !is.null(s$s0), pondere = !is.null(s$observations$DModX_pondere), pm_zero = any(m$ssxv[, 1] == 0),
       dcrit_simple = N - 1 - A >= 1 && dfm >= K && (min(K, 100) - A) / (N / (N - 1 - A)) >= 1,
       cv_anova = pls && all(n_y - 1 - 2 * A > 0),
       dmodx_affiche = if (!is.null(s$observations$DModX_pondere) && (is.null(d$version_origine) || isTRUE(d$version_origine >= 13))) "DModX_pondere" else "DModX",
       test = if (length(test)) list(manquantes = anyNA(bt), vides = any(presentes_test == 0), ki_court = any(presentes_test <= A),
                                     erreurs_manquantes = any(presentes_test == 0) || anyNA(yt)))
}

# Les termes d'un bloc (X ou Y) : nom, variable (colonne du tableau du script) et catégorie d'un
# terme simple, facteurs d'un terme croisé, transformation (et s'il faut écarter des valeurs
# hors de son domaine), mise à l'échelle (celle de `mb`, le bloc du modèle ajusté). `cles` : les
# termes simples par « variable catégorie », comme construire_bloc() les cherche pour les
# facteurs des termes croisés (ceux de X d'abord pour le bloc Y).
plan_bloc <- function(projet, termes, mb, tab, obs, bloc, cles = list()) {
  uids <- vapply(projet$variables, function(v) as.numeric(v$uid), 0)
  sortie <- vector("list", length(termes))
  for (k in seq_along(termes)) {
    t <- termes[[k]]
    p <- list(nom = mb$noms[k], croise = length(t$variables) > 1, tr = t$transform, type = mb$type[k],
              centre = mb$centre[k], poids = mb$poids[k], facteur = mb$facteur[k] %||% 1, bloc = bloc, k = k,
              bloc_ponderation = if (length(t$scaling$block)) as.character(t$scaling$block[1]))
    if (!p$croise) {
      v <- variable_du_projet(projet, t$variables[1])
      p$colonne <- unname(tab$noms[as.character(v$uid)])
      p$categorie <- categorie(t)
      p$qualitative <- length(v$categories) > 0
      p$etiquette <- if (p$categorie != 0 && p$qualitative) as.character(v$categories[p$categorie])
      p$position <- match(as.numeric(v$uid), uids)
      p$garde <- !is.null(t$transform) && hors_domaine(t$transform, valeurs_simples(projet, t, obs))
      cles[[paste(t$variables[1], p$categorie)]] <- p[c("bloc", "k", "nom")]
    }
    sortie[[k]] <- p
  }
  for (k in which(vapply(sortie, `[[`, TRUE, "croise"))) {
    t <- termes[[k]]
    sortie[[k]]$facteurs <- lapply(seq_along(t$variables), function(j) cles[[paste(t$variables[j], categorie(t, j))]])
    sortie[[k]]$garde <- !is.null(t$transform)  # (le produit n'est pas connu ici : la garde reste)
  }
  list(termes = sortie, cles = cles)
}

# Une transformation sort-elle de son domaine pour ces valeurs x (log d'un nombre ≤ 0, racine d'un
# nombre négatif, puissance négative de 0) ? Ces valeurs deviennent alors manquantes.
hors_domaine <- function(tr, x) {
  p <- as.numeric(tr$params)
  base <- p[2] * x + p[3]
  type <- as.character(tr$type)
  if (type %in% c("log10", "ln", "neglog")) return(any(base <= 0, na.rm = TRUE))
  if (type == "exp") return(any(!is.finite(exp(base)) & !is.na(base)))
  if (type == "lineaire") return(FALSE)
  if (type == "logit") return(any(!(x > p[2] & x < p[3]), na.rm = TRUE))
  any((base < 0 & p[4] != round(p[4])) | (base == 0 & p[4] < 0), na.rm = TRUE)
}

# Le prétraitement des spectres d'un modèle : les étapes, et les termes X qu'elles corrigent (les
# termes simples quantitatifs, dans l'ordre), leurs zones de variables voisines dans le tableau
# (Savitzky–Golay) et leur abscisse (lue dans le nom ou place dans le tableau).
plan_spectres <- function(projet, d, termes) {
  if (!length(d$spectres)) return(NULL)
  simples <- which(!vapply(termes, `[[`, TRUE, "croise"))
  q <- simples[vapply(termes[simples], function(t) t$categorie == 0, TRUE)]
  positions <- vapply(termes[q], `[[`, 0, "position")
  noms <- vapply(termes[q], `[[`, "", "nom")
  list(etapes = d$spectres, colonnes = match(q, simples), tous = length(q) == length(simples), noms = noms,
       zones = unname(split(seq_along(q), cumsum(c(1, diff(positions) != 1)))),
       abscisses = if (identical(abscisses_spectre(noms, positions), positions)) positions else NULL)
}

# Ce dont les modèles du script ont besoin, pour choisir ses fonctions.
besoins_script <- function(plans, seance) {
  types <- vapply(plans, `[[`, "", "type")
  etapes <- unique(unlist(lapply(plans, function(pl) vapply(pl$spectres$etapes, `[[`, "", "type"))))
  reg <- Filter(function(pl) isTRUE(pl$regression), plans)
  list(acp = any(types == "acp"), pls = any(types == "pls"), opls = any(types == "opls"),
       opls_general = any(vapply(plans, function(pl) pl$type == "opls" && pl$Ap > 1, TRUE)),
       manquantes = any(vapply(plans, `[[`, TRUE, "manquantes")), y_manquantes = any(vapply(plans, `[[`, TRUE, "y_manquantes")),
       vides = any(vapply(plans, `[[`, TRUE, "vides")),
       plsda = any(vapply(plans, `[[`, TRUE, "plsda")), spectres = etapes, seance = seance,
       # les régressions : leurs fonctions, et les termes mis à l'échelle (toujours pour les autres modèles)
       regression = length(reg) > 0, lmm = any(vapply(reg, function(pl) !pl$generalise && length(pl$niveaux) > 0, TRUE)),
       mgcv = any(vapply(reg, `[[`, TRUE, "generalise")), courbes = any(vapply(reg, function(pl) length(pl$lisses) > 0, TRUE)),
       aleatoires_mgcv = any(vapply(reg, function(pl) pl$generalise && !isTRUE(pl$gamm) && length(pl$niveaux) > 0, TRUE)),
       gamm = any(vapply(reg, function(pl) isTRUE(pl$gamm), TRUE)), car1 = any(vapply(reg, function(pl) !is.null(pl$temps), TRUE)),
       # (des pentes aléatoires de nlme, dans un lme ou la partie nlme d'un mgcv::gamm : leur tableau des variances)
       pentes_nlme = any(vapply(reg, function(pl) (!pl$generalise || isTRUE(pl$gamm)) && length(unlist(lapply(pl$niveaux, `[[`, "pentes"))) > 0, TRUE)),
       residus_quantiles = any(vapply(reg, function(pl) isTRUE(pl$quantiles), TRUE)),
       dates = any(vapply(reg, function(pl) isTRUE(pl$temps$dates), TRUE)),
       selection_aic = any(vapply(reg, function(pl) !is.null(pl$selection), TRUE)),
       # (les fonctions de Nouvelle variable qu'appelle la condition d'un modèle : condition.R)
       fonctions_condition = unique(unlist(lapply(plans, function(pl) pl$condition$fonctions))),
       pentes = any(vapply(reg, function(pl) length(pl$pentes) > 0, TRUE)),
       echelle = any(vapply(plans, function(pl) !isTRUE(pl$regression) || pl$K > 0, TRUE)),
       nipals = any(vapply(plans, function(pl) !isTRUE(pl$regression), TRUE)))
}

# ---------------------------------------------------------------- le code d'un modèle

# Le code d'un modèle, en cinq morceaux : ajustement (observations, termes, mise à l'échelle,
# ajustement et validation croisée), orientation (signes des composantes, `signes`),
# statistiques, sortie (les résultats affichés) et graphiques ; `objets` : les résultats qu'il
# crée. `seance` : son numéro dans un script de séance (NULL : seul dans son script).
code_modele <- function(pl, b, langue, seance = NULL, signes = NULL) {
  tx <- function(fr, en) if (langue == "en") en else fr
  pls <- pl$type != "acp"
  test <- length(pl$rangs_test) > 0
  # X et Y mis à l'échelle de la même façon : un seul commentaire
  unique_type <- function(ts) { u <- unique(vapply(ts, `[[`, "", "type")); if (length(u) == 1) u else "" }
  ensemble <- pls && !any(vapply(pl$x, `[[`, TRUE, "croise")) && nzchar(unique_type(pl$x)) && identical(unique_type(pl$x), unique_type(pl$y))
  ajustement <- c(code_observations(pl, tx), "",
                  code_termes(pl$x, "X", pl, tx), "",
                  if (pls) c(code_termes(pl$y, "Y", pl, tx),
                             if (pl$type != "acp" && !pl$plsda) tx("y_observe <- Y  # Y dans les unités des données",
                                                                    "y_observe <- Y  # Y in the units of the data")
                             else tx("y_observe <- Y  # les indicatrices 0/1 des classes", "y_observe <- Y  # the 0/1 indicators of the classes"), ""),
                  code_pretraiter(pl, tx, ensemble), "",
                  if (test) c(code_separer_test(pl, tx), ""),
                  code_ajustement(pl, b, tx))
  objets <- c("m", "vc", "s", "pretraiter", "X", if (pls) "Y", if (!is.null(pl$groupes_vc)) "groupes_vc", "composantes", "observations", "variables", "limites",
              if (pls) "coefficients", if (pl$cv_anova) "cv_anova", if (pl$plsda) "classification",
              if (test) "predictions_test", if (test && pl$type != "acp" && !pl$plsda) "test",
              if (test && pl$plsda) "classification_test")
  list(ajustement = traduire_code(ajustement, langue),
       orientation = traduire_code(code_orientation(pl, signes, tx), langue),
       statistiques = traduire_code(c(code_statistiques(pl, tx), if (test) c("", code_test(pl, tx))), langue),
       sortie = code_sortie(pl, tx, is.null(seance), langue),
       graphiques = lapply(code_graphiques(pl, tx), function(g) { g$code <- traduire_code(g$code, langue); g }),
       objets = nom_script(objets, langue))
}

# Les observations du modèle (et du jeu de test) : le tableau `d`. Un modèle à condition les choisit
# par elle (pl$condition : condition.R, code_condition).
code_observations <- function(pl, tx) {
  n <- length(pl$rangs)
  nt <- length(pl$rangs_test)
  total <- length(pl$noms_tableau)
  if (!nt && is.null(pl$condition) && identical(as.integer(pl$rangs), seq_len(total))) {
    return(c(commentaire(tx(sprintf("Les %d observations du tableau.", n), sprintf("The %d observations of the table.", n))), "d <- donnees"))
  }
  exclues <- setdiff(seq_len(total), c(pl$rangs, pl$rangs_test))
  quelles <- if (length(exclues) && length(exclues) <= 8) liste_textes(texte_sur(pl$noms_tableau[exclues]), tx("et", "and"))
  sauf <- if (length(exclues) && !is.unsorted(pl$rangs, strictly = TRUE)) {
    if (!is.null(quelles)) tx(sprintf(" : toutes sauf %s", quelles), sprintf(": all but %s", quelles))
    else tx(sprintf(" : %d exclues", length(exclues)), sprintf(": %d excluded", length(exclues)))
  } else if (is.unsorted(pl$rangs, strictly = TRUE)) tx(", dans l'ordre du modèle", ", in the model's order") else ""
  if (!nt) {
    if (!is.null(pl$condition)) {
      return(c(code_condition(pl$condition, tx, tx(sprintf("Les %d observations du modèle%s.", n, sauf), sprintf("The %d observations of the model%s.", n, sauf))),
               "d <- donnees[lignes, ]"))
    }
    return(c(commentaire(tx(sprintf("Les %d observations du modèle (leurs rangs dans le tableau)%s.", n, sauf),
                            sprintf("The %d observations of the model (their rows in the table)%s.", n, sauf))),
             code_valeurs(morceaux_rangs(pl$rangs), "lignes <- "), "d <- donnees[lignes, ]"))
  }
  modele_test <- if (!is.null(pl$condition)) {
    c(code_condition(pl$condition, tx, tx(sprintf("Les %d observations du modèle%s.", n, sauf), sprintf("The %d observations of the model%s.", n, sauf))),
      commentaire(tx(sprintf("Les %d observations du jeu de test (leurs rangs dans le tableau), cochées à part, sans la condition : laissées hors du modèle (ni l'ajustement ni la validation croisée ne les voient), puis prédites par lui.", nt),
                     sprintf("The %d observations of the test set (their rows in the table), ticked apart, without the condition: left out of the model (neither the fit nor the cross-validation sees them), then predicted by it.", nt))))
  } else {
    c(commentaire(tx(sprintf("Les %d observations du modèle%s, puis les %d du jeu de test : laissées hors du modèle (ni l'ajustement ni la validation croisée ne les voient), puis prédites par lui. Leurs rangs dans le tableau.", n, sauf, nt),
                     sprintf("The %d observations of the model%s, then the %d of the test set: left out of the model (neither the fit nor the cross-validation sees them), then predicted by it. Their rows in the table.", n, sauf, nt))),
      code_valeurs(morceaux_rangs(pl$rangs), "lignes <- "))
  }
  c(modele_test,
    code_valeurs(morceaux_rangs(pl$rangs_test), "lignes_test <- "),
    "d <- donnees[c(lignes, lignes_test), ]",
    tx("modele <- seq_along(lignes)  # les lignes de d qui font le modèle", "modele <- seq_along(lignes)  # the rows of d that make the model"))
}

# Les termes simples d'un bloc, tels que lus dans `d` (une colonne par terme), puis transformés.
code_termes <- function(termes, M, pl, tx) {
  ts <- Filter(function(t) !t$croise, termes)
  if (!length(ts)) return(character(0))
  noms <- vapply(ts, `[[`, "", "nom")
  simple <- vapply(ts, function(t) !t$qualitative && t$categorie == 0 && !is.na(t$colonne) && identical(t$nom, t$colonne), TRUE)
  qualitatives <- unique(vapply(Filter(function(t) t$qualitative && t$categorie != 0, ts), `[[`, "", "colonne"))
  explication <- if (M == "X") {
    if (all(simple)) tx(sprintf("Les %d termes X : les colonnes du tableau, une par terme.", length(ts)), sprintf("The %d X terms: columns of the table, one per term.", length(ts)))
    else tx(sprintf("Les termes X : les colonnes du tableau%s.", if (length(qualitatives)) ", et une indicatrice 0/1 par catégorie d'une variable qualitative" else ""),
            sprintf("The X terms: columns of the table%s.", if (length(qualitatives)) ", and a 0/1 indicator for each category of a qualitative variable" else ""))
  } else if (pl$plsda) tx(sprintf("Y : une indicatrice 0/1 par classe de « %s ».", texte_sur(paste(qualitatives, collapse = ", "))),
                          sprintf("Y: a 0/1 indicator for each class of “%s”.", texte_sur(paste(qualitatives, collapse = ", "))))
  else if (length(ts) == 1) tx("Le terme Y : une colonne du tableau.", "The Y term: a column of the table.")
  else tx(sprintf("Les %d termes Y : des colonnes du tableau.", length(ts)), sprintf("The %d Y terms: columns of the table.", length(ts)))
  # les suites de colonnes prises telles quelles (as.matrix), les autres termes un par un (cbind)
  suites <- split(seq_along(ts), cumsum(c(1, diff(simple) != 0 | !simple[-1])))
  morceaux <- lapply(suites, function(i) {
    if (all(simple[i]) && length(i) > 1) return(code_colonnes(noms[i], "as.matrix(d[, ", "])", pl, tx))
    sprintf("%s = %s", code_texte(ts[[i]]$nom), valeur_terme(ts[[i]]))
  })
  construction <- if (all(simple)) {
    code_colonnes(noms, sprintf("%s <- as.matrix(d[, ", M), "])", pl, tx)
  } else {
    debut <- sprintf("%s <- cbind(", M)
    marge <- strrep(" ", nchar(debut))
    unlist(lapply(seq_along(morceaux), function(i) {
      x <- morceaux[[i]]
      x[length(x)] <- paste0(x[length(x)], if (i < length(morceaux)) "," else ")")
      paste0(c(if (i == 1) debut else marge, rep(marge, length(x) - 1)), x)
    }))
  }
  notes <- unlist(lapply(c(list(construction), morceaux), attr, "note"))
  c(commentaire(c(explication, unique(notes))), construction, code_transformations(ts, M, tx))
}

# La valeur d'un terme simple lue dans `d` : la colonne de sa variable, ou l'indicatrice 0/1 de sa
# catégorie.
valeur_terme <- function(t) {
  if (is.na(t$colonne)) return("rep(NA_real_, nrow(d))")  # une variable sans données dans le projet
  col <- sprintf("d[[%s]]", code_texte(t$colonne))
  if (t$categorie == 0) return(if (t$qualitative) sprintf("as.numeric(%s)", col) else col)
  sprintf("as.numeric(%s == %s)", col, if (t$qualitative) code_texte(t$etiquette) else texte_exact(t$categorie))
}

# Des colonnes du tableau : par leur nom, ou par leur rang quand elles sont nombreuses (la note,
# en attribut, dit lesquelles).
code_colonnes <- function(colonnes, debut, fin, pl, tx) {
  if (length(colonnes) == 1) return(paste0(debut, code_texte(colonnes), ", drop = FALSE", fin))
  if (length(colonnes) <= 40) return(code_valeurs(vapply(colonnes, code_texte, "", USE.NAMES = FALSE), debut, fin))
  premiere <- texte_sur(colonnes[1])
  derniere <- texte_sur(colonnes[length(colonnes)])
  structure(code_valeurs(morceaux_rangs(match(colonnes, pl$colonnes)), debut, fin),
            note = tx(sprintf("Les %d colonnes de « %s » à « %s » (leurs rangs dans le tableau).", length(colonnes), premiere, derniere),
                      sprintf("The %d columns from “%s” to “%s” (their ranks in the table).", length(colonnes), premiere, derniere)))
}

# Les transformations des termes, une ligne par transformation (ses termes ensemble).
code_transformations <- function(ts, M, tx) {
  avec <- which(vapply(ts, function(t) !is.null(t$tr) && !identite(t$tr), TRUE))
  if (!length(avec)) return(character(0))
  cle <- vapply(ts[avec], function(t) paste(deparse(t$tr), t$garde), "")
  noms <- vapply(ts, `[[`, "", "nom")
  unlist(lapply(split(avec, factor(cle, unique(cle))), function(ks) {
    t <- ts[[ks[1]]]
    ref <- ref_colonnes(M, ks, noms, length(ts))
    sprintf("%s <- %s  # %s", ref, code_transformation(t$tr, ref, t$garde), nom_transformation_script(t$tr, t$garde, tx))
  }), use.names = FALSE)
}

identite <- function(tr) identical(as.character(tr$type), "7") && isTRUE(all(as.numeric(tr$params)[2:4] == c(1, 0, 1)))

# Le code d'une transformation de `x` : log10, ln ou -log10 de (C1 * x + C2), exp(C1 * x + C2),
# C1 * x + C2, ln((x - C1) / (C2 - x)) (logit), ou (C1 * x + C2) ^ C3 (type 7) ; comme
# pretraitement.R : transformer. Avec `garde`, les valeurs hors du domaine deviennent manquantes
# d'abord.
code_transformation <- function(tr, x, garde) {
  p <- as.numeric(tr$params)
  type <- as.character(tr$type)
  if (type == "logit") {
    bornes <- sprintf("(%s - %s) / (%s - %s)", x, texte_exact(p[2]), texte_exact(p[3]), x)
    return(if (garde) sprintf("log(ifelse(%s > %s & %s < %s, %s, NA))", x, texte_exact(p[2]), x, texte_exact(p[3]), bornes) else sprintf("log(%s)", bornes))
  }
  base <- if (p[2] == 1) x else sprintf("%s * %s", texte_exact(p[2]), x)
  if (p[3] > 0) base <- sprintf("%s + %s", base, texte_exact(p[3]))
  if (p[3] < 0) base <- sprintf("%s - %s", base, texte_exact(-p[3]))
  if (type %in% c("log10", "ln", "neglog")) {
    f <- c(log10 = "log10(%s)", ln = "log(%s)", neglog = "-log10(%s)")[[type]]
    return(sprintf(f, if (garde) sprintf("ifelse(%s > 0, %s, NA)", base, base) else base))
  }
  if (type == "exp") return(if (garde) sprintf("ifelse(is.finite(exp(%s)), exp(%s), NA)", base, base) else sprintf("exp(%s)", base))
  if (type == "lineaire") return(base)
  e <- p[4]
  if (garde) base <- sprintf("ifelse(%s %s, %s, NA)", base, if (e != round(e)) (if (e > 0) ">= 0" else "> 0") else "!= 0", base)
  if (e == 0.5) return(sprintf("sqrt(%s)", base))
  sprintf("%s^%s", if (grepl("^[A-Za-z_.][A-Za-z0-9_.]*(\\[[^]]*\\])?$", base) || startsWith(base, "ifelse(")) base else sprintf("(%s)", base), texte_exact(e))
}

nom_transformation_script <- function(tr, garde, tx) {
  p <- as.numeric(tr$params)
  nom <- switch(as.character(tr$type),
    log10 = tx("logarithme décimal", "decimal logarithm"), ln = tx("logarithme népérien", "natural logarithm"),
    neglog = tx("moins le logarithme décimal", "minus the decimal logarithm"), exp = tx("exponentielle", "exponential"),
    lineaire = tx("transformation linéaire", "linear transformation"),
    logit = tx(sprintf("logit entre les bornes %s et %s", texte_exact(p[2]), texte_exact(p[3])), sprintf("logit between the bounds %s and %s", texte_exact(p[2]), texte_exact(p[3]))),
    if (p[4] == 0.5) tx("racine carrée", "square root") else if (p[4] == 0.25) tx("racine quatrième", "fourth root")
    else tx(sprintf("puissance %s", texte_exact(p[4])), sprintf("power %s", texte_exact(p[4]))))
  if (garde) nom <- paste(nom, tx("(hors de son domaine : manquante)", "(outside its domain: missing)"))
  nom
}

# Une colonne ou plusieurs d'une matrice : par leur nom quand les noms sont uniques, sinon par
# leur rang ; toute la matrice si ce sont toutes ses colonnes.
ref_colonnes <- function(M, ks, noms, n) {
  if (length(ks) == n && identical(as.integer(ks), seq_len(n))) return(M)
  if (!anyDuplicated(noms)) return(paste(code_valeurs(vapply(noms[ks], code_texte, "", USE.NAMES = FALSE), sprintf("%s[, ", M), "]"), collapse = " "))
  sprintf("%s[, %s]", M, paste(code_valeurs(morceaux_rangs(ks), ""), collapse = " "))
}

ref_colonne <- function(M, k, noms) if (!anyDuplicated(noms)) sprintf("%s[, %s]", M, code_texte(noms[k])) else sprintf("%s[, %d]", M, k)

# Le prétraitement des spectres, sur les termes X quantitatifs, avant la mise à l'échelle (dans
# pretraiter() : le spectre moyen du MSC est celui des lignes d'apprentissage).
code_spectres <- function(pl, tx) {
  sp <- pl$spectres
  S <- if (sp$tous) "X" else "S"
  etape <- function(e) switch(e$type,
    snv = c(tx("SNV : chaque spectre centré sur sa moyenne et divisé par son écart-type", "SNV: each spectrum centred on its mean and divided by its standard deviation"),
            sprintf("%s <- snv(%s)", S, S)),
    msc = c(tx("MSC : chaque spectre ajusté sur le spectre moyen des lignes d'apprentissage, x = a + b r, puis remplacé par (x - a) / b",
               "MSC: each spectrum fitted on the mean spectrum of the training rows, x = a + b r, then replaced by (x - a) / b"),
            sprintf("%s <- msc(%s, colMeans(%s[apprentissage, , drop = FALSE], na.rm = TRUE))", S, S, S)),
    sg = c(tx(sprintf("Savitzky–Golay : %s (fenêtre de %d points, polynôme de degré %d)%s", c("lissage", "dérivée première", "dérivée seconde", "dérivée troisième")[e$derivee + 1] %||% sprintf("dérivée %d", e$derivee),
                      e$fenetre, e$degre, if (length(sp$zones) > 1) ", sur chaque zone de variables voisines dans le tableau" else ""),
              sprintf("Savitzky–Golay: %s (%d-point window, degree-%d polynomial)%s", c("smoothing", "first derivative", "second derivative", "third derivative")[e$derivee + 1] %||% sprintf("derivative %d", e$derivee),
                      e$fenetre, e$degre, if (length(sp$zones) > 1) ", on each range of neighbouring variables in the table" else "")),
           if (length(sp$zones) == 1) sprintf("%s <- savitzky_golay(%s, fenetre = %d, degre = %d, derivee = %d)", S, S, e$fenetre, e$degre, e$derivee)
           else c(code_valeurs(vapply(sp$zones, function(z) paste(code_valeurs(morceaux_rangs(z), ""), collapse = " "), ""), "for (zone in list(", ")) {", ouvrir = ""),
                  sprintf("  %s[, zone] <- savitzky_golay(%s[, zone], fenetre = %d, degre = %d, derivee = %d)", S, S, e$fenetre, e$degre, e$derivee), "}")),
    detrend = c(tx(sprintf("detrend : un polynôme de degré %d de l'abscisse, ajusté sur chaque spectre, en est retiré", e$degre),
                   sprintf("detrend: a degree-%d polynomial of the abscissa, fitted on each spectrum, is removed from it", e$degre)),
                sprintf("%s <- detrend(%s, abscisses, degre = %d)", S, S, e$degre)),
    ligne_base = c(tx("ligne de base : l'enveloppe convexe inférieure de chaque spectre en est retirée", "baseline: the lower convex hull of each spectrum is removed from it"),
                   sprintf("%s <- ligne_base(%s, abscisses)", S, S)))
  types <- vapply(sp$etapes, `[[`, "", "type")
  c(commentaire(tx(sprintf("Prétraitement des spectres : chaque observation est un spectre (ses %d termes X quantitatifs, dans l'ordre du tableau), corrigé par ces étapes, dans l'ordre, avant la mise à l'échelle.", length(sp$colonnes)),
                   sprintf("Spectral preprocessing: each observation is a spectrum (its %d quantitative X terms, in table order), corrected by these steps, in order, before scaling.", length(sp$colonnes)))),
    if (!sp$tous) c(code_valeurs(morceaux_rangs(sp$colonnes), "spectre <- "), "S <- X[, spectre]"),
    if (any(types %in% c("detrend", "ligne_base"))) {
      if (is.null(sp$abscisses)) c(tx("# L'abscisse de chaque variable : le nombre lu dans son nom (« 1450 nm » : 1450).", "# The abscissa of each variable: the number read in its name (“1450 nm”: 1450)."),
                                   sprintf("nombres <- regmatches(colnames(%s), regexpr(\"-?[0-9]+([.,][0-9]+)?\", colnames(%s)))", S, S),
                                   "abscisses <- as.numeric(sub(\",\", \".\", nombres, fixed = TRUE))")
      else c(tx("# L'abscisse de chaque variable : sa place dans le tableau.", "# The abscissa of each variable: its place in the table."), code_valeurs(morceaux_rangs(sp$abscisses), "abscisses <- "))
    },
    unlist(lapply(sp$etapes, function(e) { x <- etape(e); c(commentaire(paste0(toupper(substr(x[1], 1, 1)), substring(x[1], 2), ".")), x[-1]) })),
    if (!sp$tous) "X[, spectre] <- S")
}

# La mise à l'échelle d'un bloc : ses termes simples, puis ses termes croisés (produits de termes
# simples déjà mis à l'échelle, mis à l'échelle à leur tour), dans l'ordre du modèle.
code_echelle_bloc <- function(termes, M, sfx, pl, tx, blocs = M) {
  simples <- which(!vapply(termes, `[[`, TRUE, "croise"))
  croises <- which(vapply(termes, `[[`, TRUE, "croise"))
  noms_s <- vapply(termes[simples], `[[`, "", "nom")
  sur <- function(V, n) sprintf("%s[apprentissage, %s]", V, if (n == 1) ", drop = FALSE" else "")
  lignes <- c(if (!is.null(blocs)) commentaire(texte_echelle(termes[simples], blocs, pl, tx)),
              code_echelle(termes[simples], M, sfx, sur(M, length(simples)), tx, pl$manquantes),
              sprintf("%s <- mettre_a_echelle(%s, centre_%s, poids_%s)", M, M, sfx, sfx))
  if (!length(croises)) return(lignes)
  C <- if (M == "X") "croises" else "croises_y"
  sc <- if (M == "X") "c" else "cy"
  noms <- vapply(termes, `[[`, "", "nom")
  facteur <- function(f) {
    if (identical(f$bloc, M)) ref_colonne(M, match(f$k, simples), noms_s)
    else ref_colonne("X", f$k, vapply(pl$x, `[[`, "", "nom"))
  }
  produits <- vapply(termes[croises], function(t) sprintf("%s = %s", code_texte(t$nom), paste(vapply(t$facteurs, facteur, ""), collapse = " * ")), "")
  avec_tr <- Filter(function(i) !is.null(termes[[croises[i]]]$tr) && !identite(termes[[croises[i]]]$tr), seq_along(croises))
  c(lignes, "",
    commentaire(tx("Termes croisés et carrés : le produit de termes déjà centrés et mis à l'échelle, mis à l'échelle à son tour.",
                   "Cross and squared terms: the product of terms already centred and scaled, scaled in turn.")),
    paste0(c(sprintf("%s <- cbind(", C), rep(strrep(" ", nchar(C) + 10), length(produits) - 1)), produits, c(rep(",", length(produits) - 1), ")")),
    unlist(lapply(avec_tr, function(i) {
      t <- termes[[croises[i]]]
      ref <- ref_colonne(C, i, noms[croises])
      sprintf("%s <- %s  # %s", ref, code_transformation(t$tr, ref, TRUE), nom_transformation_script(t$tr, TRUE, tx))
    })),
    code_echelle(termes[croises], C, sc, sur(C, length(croises)), tx, pl$manquantes),
    sprintf("%s <- cbind(%s, mettre_a_echelle(%s, centre_%s, poids_%s))", M, M, C, sc, sc),
    sprintf("centre_%s <- c(centre_%s, centre_%s)", sfx, sfx, sc),
    sprintf("poids_%s <- c(poids_%s, poids_%s)", sfx, sfx, sc),
    if (is.unsorted(c(simples, croises))) c(
      tx(sprintf("ordre_%s <- %s  # les termes dans l'ordre du modèle", sfx, paste(code_valeurs(morceaux_rangs(match(seq_along(termes), c(simples, croises))), ""), collapse = " ")),
         sprintf("ordre_%s <- %s  # the terms in the model's order", sfx, paste(code_valeurs(morceaux_rangs(match(seq_along(termes), c(simples, croises))), ""), collapse = " "))),
      sprintf("%s <- %s[, ordre_%s]", M, M, sfx), sprintf("centre_%s <- centre_%s[ordre_%s]", sfx, sfx, sfx), sprintf("poids_%s <- poids_%s[ordre_%s]", sfx, sfx, sfx)))
}

# La mise à l'échelle des termes d'un bloc, en mots : la plus fréquente (celle de base de
# code_echelle) ; les autres sont dites à côté de leurs termes.
texte_echelle <- function(ts, blocs, pl, tx) {
  types <- vapply(ts, `[[`, "", "type")
  base <- type_echelle_base(types)
  sens <- switch(base,
    UV = tx("chacun centré sur sa moyenne et divisé par son écart-type (UV, variance unité)", "each centred on its mean and divided by its standard deviation (UV, unit variance)"),
    Par = tx("chacun centré sur sa moyenne et divisé par la racine de son écart-type (Pareto)", "each centred on its mean and divided by the square root of its standard deviation (Pareto)"),
    Ctr = tx("chacun centré sur sa moyenne seulement", "each centred on its mean only"),
    none = tx("ni centrés ni mis à l'échelle", "neither centred nor scaled"),
    fixe = tx("avec les centres et poids enregistrés dans le projet", "with the centres and weights stored in the project"))
  sauf <- if (any(types != base)) tx(", sauf ceux qui suivent", ", except those below") else ""
  tx(sprintf("Mise à l'échelle des termes %s : %s%s.", paste(blocs, collapse = " et "), sens, sauf),
     sprintf("Scaling of the %s terms: %s%s.", paste(blocs, collapse = " and "), sens, sauf))
}

type_echelle_base <- function(types) {
  connus <- table(factor(types[types != "fixe"], c("UV", "Par", "Ctr", "none")))
  if (sum(connus)) names(connus)[which.max(connus)] else "fixe"
}

# Centres et poids des termes d'un bloc, d'après leur mise à l'échelle (UV, Pareto, centrage
# seul, aucune, ou enregistrée dans le projet), calculés sur `sur` (les lignes d'apprentissage).
# Sur les lignes d'un sous-modèle de la validation croisée, un terme peut être constant (poids 0),
# ou, avec des valeurs manquantes (`na`), n'avoir aucune valeur (centre 0) : comme dans Nuée.
code_echelle <- function(ts, M, sfx, sur, tx, na) {
  types <- vapply(ts, `[[`, "", "type")
  noms <- vapply(ts, `[[`, "", "nom")
  ref <- function(ks) paste(if (!anyDuplicated(noms)) code_valeurs(vapply(noms[ks], code_texte, "", USE.NAMES = FALSE), "")
                            else code_valeurs(morceaux_rangs(ks), ""), collapse = " ")
  fixe <- function(ks, quoi) code_nombres(vapply(ts[ks], `[[`, 0, quoi))
  centre <- sprintf("centre_%s", sfx)
  poids <- sprintf("poids_%s", sfx)
  if (all(types == "fixe")) {
    return(c(code_valeurs(texte_exact(vapply(ts, `[[`, 0, "centre")), sprintf("%s <- ", centre)),
             code_valeurs(texte_exact(vapply(ts, `[[`, 0, "poids")), sprintf("%s <- ", poids))))
  }
  base <- type_echelle_base(types)  # la plus fréquente ; les autres ensuite, terme par terme
  autres <- setdiff(unique(types), base)
  moyennes <- any(types %in% c("UV", "Par", "Ctr"))
  ecart <- if (any(autres %in% c("UV", "Par"))) sprintf("ecart_%s", sfx)
  et <- sprintf("apply(%s, 2, sd, na.rm = TRUE)", sur)
  un <- function(v) if (length(autres)) sprintf("setNames(rep(%s, ncol(%s)), colnames(%s))", v, M, M) else sprintf("rep(%s, ncol(%s))", v, M)
  note <- function(ligne, fr, en) paste0(ligne, "  # ", tx(fr, en))
  lignes <- c(if (moyennes) sprintf("%s <- apply(%s, 2, mean, na.rm = TRUE)", centre, sur) else sprintf("%s <- %s", centre, un("0")),
              if (!is.null(ecart)) sprintf("%s <- %s", ecart, et),
              sprintf("%s <- %s", poids, switch(base, UV = sprintf("1 / %s", ecart %||% et), Par = sprintf("1 / sqrt(%s)", ecart %||% et), un("1"))),
              if (base == "none" && moyennes) sprintf("%s[%s] <- 0", centre, ref(which(types == "none"))))
  for (ty in autres) {
    ks <- which(types == ty)
    r <- ref(ks)
    lignes <- c(lignes, switch(ty,
      UV = note(sprintf("%s[%s] <- 1 / %s[%s]", poids, r, ecart, r), "UV : divisé par l'écart-type", "UV: divided by the standard deviation"),
      Par = note(sprintf("%s[%s] <- 1 / sqrt(%s[%s])", poids, r, ecart, r), "Pareto : divisé par la racine de l'écart-type", "Pareto: divided by the square root of the standard deviation"),
      Ctr = note(sprintf("%s[%s] <- 1", poids, r), "centré seulement", "centred only"),
      none = c(if (moyennes) note(sprintf("%s[%s] <- 0", centre, r), "ni centré ni mis à l'échelle", "neither centred nor scaled"),
               if (moyennes) sprintf("%s[%s] <- 1", poids, r) else note(sprintf("%s[%s] <- 1", poids, r), "ni centré ni mis à l'échelle", "neither centred nor scaled")),
      fixe = c(note(sprintf("%s[%s] <- %s", centre, r, fixe(ks, "centre")), "centre et poids enregistrés dans le projet", "centre and weight stored in the project"),
               sprintf("%s[%s] <- %s", poids, r, fixe(ks, "poids")))))
  }
  constants <- any(types %in% c("UV", "Par"))
  vides <- na && any(types %in% c("UV", "Par", "Ctr"))
  c(lignes,
    if (constants) note(sprintf("%s[!is.finite(%s)] <- 0", poids, poids), "terme constant : poids 0", "constant term: weight 0"),
    if (vides) note(sprintf("%s[is.nan(%s)] <- 0", centre, centre), "terme sans aucune valeur : centre 0", "term without any value: centre 0"),
    code_ponderation(ts, poids, tx))
}

# La pondération des termes (pretraitement.R : facteurs_ponderation) : leur poids multiplié par
# leur modificateur, et par 1/racine(K) pour les K termes d'un bloc ; rien si aucun n'en a.
code_ponderation <- function(ts, poids, tx) {
  f <- vapply(ts, function(t) as.numeric(t$facteur %||% 1), 0)
  if (all(f == 1)) return(character(0))
  blocs <- vapply(ts, function(t) t$bloc_ponderation %||% NA_character_, "")
  K <- table(blocs[f != 1 & !is.na(blocs)])
  detail <- if (length(K)) paste(sprintf(tx("bloc %s : %d termes, 1/racine(%d) = %s", "block %s: %d terms, 1/sqrt(%d) = %s"),
                                         names(K), K, K, signif(1 / sqrt(as.numeric(K)), 4)), collapse = " ; ")
  c(commentaire(tx(sprintf("Pondération : le poids de chaque terme multiplié par son modificateur, et par 1/racine(K) pour les K termes d'un bloc (pondération par blocs : chaque bloc pèse autant, quel que soit son nombre de termes)%s.",
                           if (!is.null(detail)) paste0(" ; ", detail) else ""),
                   sprintf("Weighting: each term's weight multiplied by its modifier, and by 1/sqrt(K) for the K terms of a block (block scaling: each block weighs the same, whatever its number of terms)%s.",
                           if (!is.null(detail)) paste0("; ", detail) else ""))),
    code_valeurs(texte_exact(f), sprintf("%s <- %s * ", poids, poids)))
}

# Le prétraitement des termes, en une fonction que l'ajustement et chaque sous-modèle de la
# validation croisée appliquent : ce qui se calcule à partir des observations (spectre moyen du
# MSC, centres et poids, termes croisés) l'est d'après les lignes `apprentissage`, puis appliqué à
# toutes les lignes.
code_pretraiter <- function(pl, tx, ensemble) {
  pls <- pl$type != "acp"
  croises <- any(vapply(pl$x, `[[`, TRUE, "croise"))
  corps <- c(if (length(pl$spectres)) c(code_spectres(pl, tx), ""),
             code_echelle_bloc(pl$x, "X", "x", pl, tx, if (ensemble) c("X", "Y") else "X"),
             if (pls) c(if (!ensemble) "", code_echelle_bloc(pl$y, "Y", "y", pl, tx, if (ensemble) NULL else "Y")),
             if (pls) "list(X = X, Y = Y, centre_x = centre_x, poids_x = poids_x, centre_y = centre_y, poids_y = poids_y)"
             else "list(X = X, centre_x = centre_x, poids_x = poids_x)")
  quoi <- c(if (length(pl$spectres)) tx("le prétraitement des spectres", "the spectral preprocessing"),
            tx("la mise à l'échelle", "the scaling"), if (croises) tx("les termes croisés", "the cross terms"))
  c(commentaire(tx(sprintf("Le prétraitement des termes (%s) : ce qu'il calcule à partir des observations l'est d'après les lignes `apprentissage` (toutes celles du modèle, ou celles d'un sous-modèle de la validation croisée), puis appliqué à toutes les lignes. Il rend %s prétraités, avec leurs centres et poids.",
                           liste_textes(quoi, "et"), if (pls) "X et Y" else "X"),
                   sprintf("The preprocessing of the terms (%s): what it computes from the observations comes from the rows `training` (all those of the model, or those of a sub-model of the cross-validation), then is applied to all the rows. It returns %s preprocessed, with their centres and weights.",
                           liste_textes(quoi, "and"), if (pls) "X and Y" else "X"))),
    sprintf("pretraiter <- function(%s, apprentissage) {", if (pls) "X, Y" else "X"),
    ifelse(nzchar(corps), paste0("  ", corps), ""),
    "}")
}

# Avec un jeu de test : ses lignes, prétraitées comme celles du modèle (d'après elles), mises à part.
code_separer_test <- function(pl, tx) {
  pls <- pl$type != "acp"
  c(commentaire(tx("Le jeu de test à part : prétraité comme le modèle, avec ce que pretraiter() calcule sur les lignes du modèle.",
                   "The test set apart: preprocessed as the model, with what preprocess() computes on the model's rows.")),
    sprintf("X_test <- pretraiter(%s, modele)$X[-modele, , drop = FALSE]", if (pls) "X, Y" else "X"), "X <- X[modele, , drop = FALSE]",
    if (pls) c("y_test <- y_observe[-modele, , drop = FALSE]", "y_observe <- y_observe[modele, , drop = FALSE]", "Y <- Y[modele, , drop = FALSE]"))
}

# L'ajustement et la validation croisée (fonctions du début du script).
code_ajustement <- function(pl, b, tx) {
  A <- pl$A
  G <- pl$G
  general <- pl$type == "opls" && b$opls_general
  groupes <- arguments_groupes(pl)
  appel <- switch(pl$type,
    acp = c("m <- acp(s$X, A)", sprintf("vc <- validation_croisee_acp(X, A, pretraiter, %s)", groupes)),
    pls = c("m <- pls(s$X, s$Y, A)", sprintf("vc <- validation_croisee_pls(X, Y, A, pretraiter, %s)", groupes)),
    opls = if (general) c(sprintf("Ap <- %d  # %s", pl$Ap, tx("composantes prédictives", "predictive components")),
                          "m <- opls(s$X, s$Y, Ap, A - Ap)", sprintf("vc <- validation_croisee_opls(X, Y, Ap, A - Ap, pretraiter, %s)", groupes))
           else c("m <- opls(s$X, s$Y, A - 1)", sprintf("vc <- validation_croisee_opls(X, Y, A - 1, pretraiter, %s)", groupes)))
  quoi <- if (pl$type == "opls") {
    Ao <- A - pl$Ap
    tx(sprintf("%d composante%s prédictive%s et %d orthogonale%s", pl$Ap, if (pl$Ap > 1) "s" else "", if (pl$Ap > 1) "s" else "", Ao, if (Ao > 1) "s" else ""),
       sprintf("%d predictive and %d orthogonal component%s", pl$Ap, Ao, if (Ao > 1) "s" else ""))
  } else tx(sprintf("%d composante%s", A, if (A > 1) "s" else ""), sprintf("%d component%s", A, if (A > 1) "s" else ""))
  par <- if (!is.null(pl$groupes_vc)) {
    if (pl$vc_fichier) tx(", ceux du fichier d'origine (groupes_vc : le groupe de chaque observation)", ", those of the original file (cv_groups: each observation's group)")
    else tx(sprintf(", un par valeur de « %s » (groupes_vc : le groupe de chaque observation, les observations de même valeur ensemble)", texte_sur(pl$vc_par)),
            sprintf(", one per value of \"%s\" (cv_groups: each observation's group, the observations with the same value together)", texte_sur(pl$vc_par)))
  } else ""
  c(commentaire(tx(sprintf("Ajustement : %s ; validation croisée en %d groupes%s, chacun prédit par un modèle ajusté sans lui (prétraitement compris).", quoi, G, par),
                   sprintf("Fit: %s; cross-validation in %d groups%s, each predicted by a model fitted without it (preprocessing included).", quoi, G, par))),
    if (!is.null(pl$groupes_vc)) code_groupes_vc(pl$groupes_vc),
    sprintf("A <- %d", A),
    sprintf("s <- pretraiter(%s, seq_len(nrow(X)))  # %s", if (pl$type != "acp") "X, Y" else "X",
            tx("les termes du modèle, prétraités d'après toutes ses lignes", "the model's terms, preprocessed from all its rows")),
    "N <- nrow(s$X)", "K <- ncol(s$X)", appel)
}

# Les arguments de groupes des fonctions de validation croisée : G, et le groupe de chaque
# observation quand ils ne sont pas à tour de rôle (`liste` : la liste des résultats du modèle,
# dans un script de séance).
arguments_groupes <- function(pl, liste = NULL) {
  if (is.null(pl$groupes_vc)) return(sprintf("G = %d", pl$G))
  sprintf("G = %d, groupe = %sgroupes_vc", pl$G, if (is.null(liste)) "" else paste0(liste, "$"))
}

# Le groupe de chaque observation du modèle, écrit en répétitions quand elles se suivent.
code_groupes_vc <- function(g) {
  r <- rle(as.integer(g))
  if (2 * length(r$values) >= length(g)) return(code_valeurs(as.character(g), "groupes_vc <- "))
  debut <- "groupes_vc <- rep("
  c(code_valeurs(as.character(r$values), debut, ","),
    code_valeurs(as.character(r$lengths), strrep(" ", nchar(debut)), ")"))
}

# Les composantes de signe opposé à celles de Nuée, retournées.
code_orientation <- function(pl, signes, tx) {
  if (!any(signes < 0)) return(character(0))
  matrices <- if (pl$type == "acp") "c(\"T\", \"P\")" else "c(\"T\", \"P\", \"W\", \"C\")"
  c(commentaire(tx("Le signe d'une composante est arbitraire : ceux-ci donnent les mêmes graphiques que dans Nuée.",
                   "The sign of a component is arbitrary: these give the same plots as in Nuée.")),
    sprintf("signes <- %s", code_nombres(signes)),
    sprintf("for (k in %s) m[[k]] <- sweep(m[[k]], 2, signes, \"*\")", matrices))
}

# R2, Q2, T2, DModX et leurs limites ; pour une PLS les VIP, les coefficients, Y ajusté, la
# CV-ANOVA et la classification d'une PLS-DA ; les tableaux de résultats.
code_statistiques <- function(pl, tx) {
  pls <- pl$type != "acp"
  na <- pl$manquantes
  vides <- pl$vides
  n <- if (vides) "Nt" else "N"
  denominateur <- if (pl$ki_court) "ifelse(Ki > A, Ki - A, NA)" else if (na) "(Ki - A)" else "(K - A)"
  noms_obs <- if (length(pl$rangs_test)) "d[[\"Observation\"]][modele]" else "d[[\"Observation\"]]"
  dmodx <- if (pl$dmodx) c(", DModX", if (pl$pondere) ", DModX_pondere") else ""
  K <- pl$K
  c(commentaire(if (pls) tx("R2 : la part de la variation de X et de Y que chaque composante explique ; Q2cum : la part de Y prédite en validation croisée, 1 - PRESS / SS, et Q2 ce qu'y ajoute chaque composante, 1 - PRESS / PRESS de la précédente ; valeur propre : R2X × min(N, K).",
                            "R2: the share of the variation of X and Y that each component explains; Q2cum: the share of Y predicted in cross-validation, 1 - PRESS / SS, and Q2 what each component adds to it, 1 - PRESS / PRESS of the previous one; eigenvalue: R2X × min(N, K).")
                else tx("R2 : la part de la variation de X que chaque composante explique ; Q2cum : la part de X prédite en validation croisée, 1 - PRESS / SS, et Q2 ce qu'y ajoute chaque composante, 1 - PRESS / PRESS de la précédente ; valeur propre : R2X × min(N, K).",
                        "R2: the share of the variation of X that each component explains; Q2cum: the share of X predicted in cross-validation, 1 - PRESS / SS, and Q2 what each component adds to it, 1 - PRESS / PRESS of the previous one; eigenvalue: R2X × min(N, K).")),
    "a <- 1:A",
    "ssx <- colSums(m$ssx)",
    if (pls) "ssy <- colSums(m$ssy)",
    "rapport <- rowSums(vc$press) / rowSums(vc$ss)",
    "composantes <- data.frame(composante = a, R2X = (ssx[a] - ssx[a + 1]) / ssx[1], R2Xcum = 1 - ssx[a + 1] / ssx[1],",
    if (pls) "                          R2Y = (ssy[a] - ssy[a + 1]) / ssy[1], R2Ycum = 1 - ssy[a + 1] / ssy[1],",
    "                          Q2 = 1 - rapport, Q2cum = 1 - rowSums(vc$press) / sum(vc$ss[1, ]))",
    "composantes$valeur_propre <- composantes$R2X * min(N, K)",
    "",
    commentaire(tx("T2 de Hotelling : la distance de chaque observation au centre du modèle, dans l'espace des scores (chaque score rapporté à sa variance) ; ses limites à 95 % et 99 %.",
                   "Hotelling's T2: the distance of each observation to the centre of the model, in the space of the scores (each score over its variance); its limits at 95% and 99%.")),
    if (vides) c(tx("notes <- !is.na(m$T[, 1])  # les observations qui ont des scores (une valeur X au moins)",
                    "notes <- !is.na(m$T[, 1])  # the observations that have scores (one X value at least)"), "Nt <- sum(notes)"),
    sprintf("var_t <- colSums(m$T%s^2) / (%s - 1)", if (vides) "[notes, , drop = FALSE]" else "", n),
    "T2 <- rowSums(sweep(m$T^2, 2, var_t, \"/\"))",
    sprintf("T2Crit <- A * (%s - 1) / (%s - A) * qf(c(0.95, 0.99), A, %s - A)", n, n, n),
    if (pl$dmodx) c("",
      commentaire(c(tx("DModX : l'écart-type des résidus X de chaque observation, rapporté à celui du modèle (s0) et corrigé de ce que l'observation a servi à l'ajuster.",
                       "DModX: the standard deviation of the X residuals of each observation, over that of the model (s0), corrected for the observation having been used to fit it."),
                    if (pl$pondere) tx("Pondéré (ce que Nuée montre d'abord) : chaque résidu multiplié par le pouvoir de modélisation de son terme.",
                                       "Weighted (what Nuée shows first): each residual multiplied by the modelling power of its term."),
                    tx("DCrit : la limite de DModX à 95 %.", "DCrit: the limit of DModX at 95%."))),
      if (na) tx("Ki <- rowSums(!is.na(s$X))  # le nombre de termes X présents pour chaque observation", "Ki <- rowSums(!is.na(s$X))  # the number of X terms present for each observation"),
      "correction <- sqrt(N / (N - A - 1))",
      "s0 <- sqrt(ssx[A + 1] / ((N - A - 1) * (K - A)))",
      sprintf("DModX <- sqrt(rowSums(m$E^2) / %s) * correction / s0", denominateur),
      if (pl$pondere) c(
        "pouvoir <- 1 - sqrt(m$ssx[, A + 1] / (N - A - 1)) / sqrt(m$ssx[, 1] / (N - 1))",
        if (pl$pm_zero) tx("pouvoir[!is.finite(pouvoir)] <- 0  # terme sans variation", "pouvoir[!is.finite(pouvoir)] <- 0  # term without variation"),
        "Ep <- sweep(m$E, 2, pouvoir, \"*\")",
        sprintf("DModX_pondere <- sqrt(rowSums(Ep^2) / %s) * correction / sqrt(sum(Ep^2) / ((N - A - 1) * (K - A)))", denominateur)),
      if (pl$dcrit_simple) sprintf("DCrit <- sqrt(qf(0.95, (%s - A) / (N / (N - 1 - A)), (N - 1 - A) * (K - A)))%s", if (K <= 100) "K" else "100",
                                   if (K > 100) tx("  # 100 termes comptés au plus", "  # 100 terms counted at most") else "")
      else c("df_mod <- max((N - 1 - A) * (K - A), 1)",
             "df_obs <- ((if (K > df_mod) min(K, 100, df_mod) + sqrt(K - df_mod) else min(K, 100, df_mod)) - A) / (N / max(N - 1 - A, 1))",
             "DCrit <- sqrt(qf(0.95, max(df_obs, 1), df_mod))")),
    if (pls) c("",
      commentaire(tx("VIP : l'importance de chaque terme X pour Y, d'après ses poids w, chaque composante comptant pour la part de Y qu'elle explique.",
                     "VIP: the importance of each X term for Y, from its weights w, each component counting for the share of Y it explains.")),
      "part_y <- ssy[a] - ssy[a + 1]",
      "VIP <- sqrt(K * rowSums(sweep(m$W^2, 2, part_y, \"*\")) / sum(part_y))", "",
      commentaire(tx("Coefficients de régression : B pour les termes centrés et mis à l'échelle, B = W (P'W)^-1 C' ; puis dans les unités des données (celles des termes transformés), avec la constante.",
                     "Regression coefficients: B for the centred and scaled terms, B = W (P'W)^-1 C'; then in the units of the data (those of the transformed terms), with the constant.")),
      "B <- m$W %*% solve(crossprod(m$P, m$W)) %*% t(m$C)",
      "coefficients <- B * s$poids_x / rep(s$poids_y, each = K)",
      "coefficients <- rbind(coefficients, Constante = s$centre_y - colSums(coefficients * s$centre_x))",
      if (!pl$plsda) c(tx("# Y ajusté par le modèle, dans les unités des données", "# Y fitted by the model, in the units of the data"),
                       "y_ajuste <- sweep(sweep(m$T %*% t(m$C), 2, s$poids_y, \"/\"), 2, s$centre_y, \"+\")")),
    if (pl$cv_anova) c("",
      commentaire(tx("CV-ANOVA (Eriksson et al., 2008), pour chaque Y : ses résidus de validation croisée comparés à sa variation autour de sa moyenne (Y centré et mis à l'échelle) ; la régression compte 2 degrés de liberté par composante.",
                     "CV-ANOVA (Eriksson et al., 2008), for each Y: its cross-validation residuals compared with its variation around its mean (Y centred and scaled); the regression counts 2 degrees of freedom per component.")),
      "cv_anova <- lapply(seq_len(ncol(s$Y)), function(j) {",
      if (pl$y_manquantes || vides) c(sprintf("  ok <- !is.na(s$Y[, j])%s", if (vides) " & notes" else ""), "  y <- s$Y[ok, j]",
                                      "  ss_total <- sum((y - mean(y))^2)", "  ss_residus <- sum((y - vc$y_vc[ok, j])^2)")
      else c("  y <- s$Y[, j]", "  ss_total <- sum((y - mean(y))^2)", "  ss_residus <- sum((y - vc$y_vc[, j])^2)"),
      "  ss <- c(ss_total, ss_total - ss_residus, ss_residus)",
      "  dl <- c(length(y) - 1, 2 * A, length(y) - 1 - 2 * A)",
      "  f <- (ss[2] / dl[2]) / (ss[3] / dl[3])",
      "  data.frame(source = c(\"Total corr.\", \"Regression\", \"Residual\"), SS = ss, DF = dl, MS = ss / dl,",
      "             F = c(NA, f, NA), p = c(NA, pf(f, dl[2], dl[3], lower.tail = FALSE), NA))",
      "})",
      "names(cv_anova) <- colnames(s$Y)"),
    if (pl$plsda) c("",
      commentaire(tx(sprintf("%s : la classe de chaque observation (celle de son Y le plus grand) et celle que prédit la validation croisée ; la table de classification.", type_script(pl, "fr")),
                     sprintf("%s: the class of each observation (that of its largest Y) and the one the cross-validation predicts; the classification table.", type_script(pl, "en")))),
      "classe <- la_plus_grande(y_observe)",
      "y_vc <- sweep(sweep(vc$y_vc, 2, s$poids_y, \"/\"), 2, s$centre_y, \"+\")",
      if (vides) "y_vc[!notes, ] <- NA",
      "classe_vc <- la_plus_grande(y_vc)",
      "classification <- table(observee = factor(classe, colnames(s$Y)), predite = factor(classe_vc, colnames(s$Y)))"),
    "",
    commentaire(tx("Les résultats en tableaux : une ligne par observation, une ligne par terme X.", "The results as tables: one row per observation, one row per X term.")),
    sprintf("observations <- data.frame(observation = %s, nommer(m$T, \"t\"), T2%s, row.names = NULL)", noms_obs, paste(dmodx, collapse = "")),
    if (pls && !pl$plsda) c(tx("observations[paste(colnames(s$Y), \"observé\")] <- y_observe", "observations[paste(colnames(s$Y), \"observed\")] <- y_observe"),
                            tx("observations[paste(colnames(s$Y), \"ajusté\")] <- y_ajuste", "observations[paste(colnames(s$Y), \"fitted\")] <- y_ajuste")),
    if (pl$plsda) c("observations$classe <- classe", "observations$classe_vc <- classe_vc"),
    sprintf("variables <- data.frame(terme = colnames(s$X), nommer(m$P, \"p\")%s, row.names = NULL)", if (pls) ", nommer(m$W, \"w\"), VIP" else ""),
    sprintf("limites <- c(\"T2Crit(95%%)\" = T2Crit[1], \"T2Crit(99%%)\" = T2Crit[2]%s)", if (pl$dmodx) ", \"DCrit(5%)\" = DCrit" else ""))
}

# Le jeu de test, projeté sur le modèle comme une nouvelle observation (prediction.R).
code_test <- function(pl, tx) {
  t <- pl$test
  pls <- pl$type != "acp"
  na <- t$manquantes
  Ao <- pl$A - pl$Ap
  ordre <- if (pl$type == "opls" && Ao > 0) paste(code_valeurs(morceaux_rangs(c(pl$Ap + seq_len(Ao), seq_len(pl$Ap))), ""), collapse = " ") else "1:A"
  v <- if (pls) "m$W[, a]" else "m$P[, a]"
  denominateur <- if (t$ki_court) "ifelse(Ki_test > A, Ki_test - A, NA)" else if (na) "(Ki_test - A)" else "(K - A)"
  hors <- if (pl$dmodx) {
    if (na) "!is.na(T2_test) & T2_test > T2Crit[1] | !is.na(DModX_test) & DModX_test > DCrit" else "T2_test > T2Crit[1] | DModX_test > DCrit"
  } else if (na) "!is.na(T2_test) & T2_test > T2Crit[1]" else "T2_test > T2Crit[1]"
  rm <- if (t$erreurs_manquantes || pl$y_manquantes) ", na.rm = TRUE" else ""
  c(commentaire(tx(sprintf("Jeu de test : chaque observation projetée sur le modèle, composante après composante%s, avec les centres et poids du modèle ; son T2, son DModX, et si elle sort du domaine du modèle (au-delà de T2Crit à 95 %%%s).",
                           if (pl$type == "opls" && Ao > 0) " (les orthogonales d'abord)" else "", if (pl$dmodx) " ou de DCrit" else ""),
                   sprintf("Test set: each observation projected on the model, component after component%s, with the model's centres and weights; its T2, its DModX, and whether it is outside the model's domain (beyond T2Crit at 95%%%s).",
                           if (pl$type == "opls" && Ao > 0) " (the orthogonal ones first)" else "", if (pl$dmodx) " or DCrit" else ""))),
    if (na) c("MT <- 1 * !is.na(X_test)", "E <- replace(X_test, is.na(X_test), 0)") else "E <- X_test",
    "T_test <- matrix(NA_real_, nrow(E), A)",
    sprintf("for (a in %s) {", ordre),
    if (na) sprintf("  T_test[, a] <- projeter(E, MT, %s)", v) else sprintf("  T_test[, a] <- drop(E %%*%% %s) / sum(%s^2)", v, v),
    if (na) "  E <- (E - outer(T_test[, a], m$P[, a])) * MT" else "  E <- E - outer(T_test[, a], m$P[, a])",
    "}",
    if (t$vides) tx("T_test[rowSums(MT) == 0, ] <- NA  # sans aucune valeur X : pas de score", "T_test[rowSums(MT) == 0, ] <- NA  # without any X value: no score"),
    "T2_test <- rowSums(sweep(T_test^2, 2, var_t, \"/\"))",
    if (pl$dmodx) c(if (na) "Ki_test <- rowSums(MT)", sprintf("DModX_test <- sqrt(rowSums(E^2) / %s) / s0", denominateur)),
    sprintf("predictions_test <- data.frame(observation = d[[\"Observation\"]][-modele], nommer(T_test, \"t\"), T2 = T2_test,%s", if (pl$dmodx) " DModX = DModX_test," else ""),
    sprintf("                               hors_domaine = %s, row.names = NULL)", hors),
    if (pls) "y_predit <- sweep(sweep(T_test %*% t(m$C), 2, s$poids_y, \"/\"), 2, s$centre_y, \"+\")",
    if (pls && !pl$plsda) c(
      commentaire(tx("Pour chaque Y : RMSEP (racine de la moyenne des carrés des erreurs de prédiction), biais (moyenne des erreurs), SEP (leur écart-type) et Q2 externe (1 - somme des carrés des erreurs / somme des carrés des écarts à la moyenne des observations du modèle).",
                     "For each Y: RMSEP (root mean square of the prediction errors), bias (mean of the errors), SEP (their standard deviation) and external Q2 (1 - sum of squared errors / sum of squared deviations from the mean of the model's observations).")),
      "erreur <- y_predit - y_test",
      if (t$erreurs_manquantes) "y_test[is.na(erreur)] <- NA",
      sprintf("test <- data.frame(y = colnames(s$Y), n = colSums(!is.na(erreur)), RMSEP = sqrt(colMeans(erreur^2%s)), biais = colMeans(erreur%s),", rm, rm),
      sprintf("                   SEP = apply(erreur, 2, sd%s), Q2ext = 1 - colSums(erreur^2%s) / colSums(sweep(y_test, 2, colMeans(y_observe%s))^2%s),", rm, rm, rm, rm),
      "                   row.names = NULL)",
      tx("predictions_test[paste(colnames(s$Y), \"observé\")] <- y_test", "predictions_test[paste(colnames(s$Y), \"observed\")] <- y_test"),
      tx("predictions_test[paste(colnames(s$Y), \"prédit\")] <- y_predit", "predictions_test[paste(colnames(s$Y), \"predicted\")] <- y_predit")),
    if (pl$plsda) c(
      commentaire(tx(sprintf("%s : la classe prédite (celle du plus grand Y prédit) contre la vraie classe.", type_script(pl, "fr")),
                     sprintf("%s: the predicted class (that of the largest predicted Y) against the true class.", type_script(pl, "en")))),
      "predictions_test$classe <- la_plus_grande(y_test)",
      "predictions_test$classe_predite <- la_plus_grande(y_predit)",
      "classification_test <- table(observee = factor(predictions_test$classe, colnames(s$Y)), predite = factor(predictions_test$classe_predite, colnames(s$Y)))"))
}

# Les résultats affichés (la partie 4 du script) ; l'appel du test de permutation en commentaire.
code_sortie <- function(pl, tx, seul, langue) {
  pls <- pl$type != "acp"
  ligne <- function(code, fr, en) {
    code <- traduire_code(code, langue)
    paste0(formatC(code, width = -max(27, nchar(code) + 2)), "# ", tx(fr, en))
  }
  test <- length(pl$rangs_test) > 0
  c(ligne("print(composantes)", if (pls) "R2X, R2Y, Q2 et valeur propre, par composante" else "R2X, Q2 et valeur propre, par composante",
          if (pls) "R2X, R2Y, Q2 and eigenvalue, by component" else "R2X, Q2 and eigenvalue, by component"),
    ligne("print(head(observations))", if (pls && !pl$plsda) "scores, T2, DModX, Y observé et ajusté" else if (pl$plsda) "scores, T2, DModX, classe observée et prédite" else "scores, T2, DModX",
          if (pls && !pl$plsda) "scores, T2, DModX, observed and fitted Y" else if (pl$plsda) "scores, T2, DModX, observed and predicted class" else "scores, T2, DModX"),
    ligne("print(variables)", if (pls) "loadings p, poids w, VIP" else "loadings p", if (pls) "loadings p, weights w, VIP" else "loadings p"),
    if (pls) ligne("print(coefficients)", "coefficients, dans les unités des données", "coefficients, in the units of the data"),
    ligne("print(limites)", "T2Crit (95 %, 99 %), DCrit (5 %)", "T2Crit (95%, 99%), DCrit (5%)"),
    if (pl$cv_anova) ligne("print(cv_anova)", "CV-ANOVA", "CV-ANOVA"),
    if (pl$plsda) ligne("print(classification)", "table de classification (validation croisée)", "classification table (cross-validation)"),
    if (test && pls && !pl$plsda) ligne("print(test)", "jeu de test : RMSEP, biais, SEP, Q2 externe", "test set: RMSEP, bias, SEP, external Q2"),
    if (test && pl$plsda) ligne("print(classification_test)", "jeu de test : classes prédites", "test set: predicted classes"),
    if (test) ligne("print(head(predictions_test))", "jeu de test : scores, T2, DModX, hors du domaine", "test set: scores, T2, DModX, outside the domain"),
    if (pls && seul) appel_permutations(pl, tx))
}

appel_permutations <- function(pl, tx, liste = NULL) {
  if (is.function(tx)) {
    appel <- switch(pl$type, pls = "permutations(X, Y, A)",
                    opls = if (pl$Ap > 1) "permutations(X, Y, Ap, A - Ap)" else "permutations(X, Y, A - 1)")
    return(paste0(formatC(paste("#", appel), width = -max(27, nchar(appel) + 4)), "# ", tx("test de permutation (quelques secondes)", "permutation test (a few seconds)")))
  }
  langue <- tx
  ajuster <- switch(pl$type, pls = sprintf("pls(s$X, s$Y, %d)", pl$A),
                    opls = if (pl$Ap > 1) sprintf("opls(s$X, s$Y, %d, %d)", pl$Ap, pl$A - pl$Ap) else sprintf("opls(s$X, s$Y, %d)", pl$A - 1))
  g <- arguments_groupes(pl, liste)
  vc <- switch(pl$type, pls = sprintf("validation_croisee_pls(%s$X, Yp, %d, %s$pretraiter, %s)", liste, pl$A, liste, g),
               opls = if (pl$Ap > 1) sprintf("validation_croisee_opls(%s$X, Yp, %d, %d, %s$pretraiter, %s)", liste, pl$Ap, pl$A - pl$Ap, liste, g)
                      else sprintf("validation_croisee_opls(%s$X, Yp, %d, %s$pretraiter, %s)", liste, pl$A - 1, liste, g))
  appel <- traduire_code(c("permutations(function(Yp) {", sprintf("  s <- %s$pretraiter(%s$X, Yp, seq_len(nrow(Yp)))", liste, liste),
                           sprintf("  list(%s, %s)", ajuster, vc), sprintf("}, %s$Y%s)", liste, if (pl$plsda) ", classes = TRUE" else "")), langue)
  c(if (langue == "en") "# Permutation test (a few seconds):" else "# Test de permutation (quelques secondes) :", paste("#", appel))
}

# Les graphiques en R de base : scores, loadings, VIP, Y observé et ajusté, T2 et DModX.
code_graphiques <- function(pl, tx) {
  pls <- pl$type != "acp"
  A <- pl$A
  Ao <- A - pl$Ap
  deux <- if (pl$type == "opls") (if (Ao > 0) pl$Ap + 1 else if (A > 1) 2 else NA) else if (A > 1) 2 else NA
  nom2 <- if (pl$type == "opls" && Ao > 0) c("to[1]", "po[1]") else c("t[2]", "p[2]")
  couleur <- if (pl$plsda) ", col = couleur" else ""
  legende <- if (pl$plsda) "legend(\"topright\", legend = colnames(s$Y), col = seq_len(ncol(s$Y)), pch = 19, bty = \"n\", cex = 0.8)"
  spectres <- length(pl$spectres) > 0
  graphes <- list(
    list(nom = "scores", largeur = 6, hauteur = 5, code = c(
      if (pl$plsda) "couleur <- as.integer(factor(classe, colnames(s$Y)))",
      if (!is.na(deux)) c(sprintf("plot(m$T[, 1], m$T[, %d], pch = 19%s, xlab = \"t[1]\", ylab = %s, main = \"Scores\")", deux, couleur, code_texte(nom2[1])),
                          "abline(h = 0, v = 0, col = \"grey\")")
      else c(sprintf("plot(m$T[, 1], pch = 19%s, xlab = \"Observation\", ylab = \"t[1]\", main = \"Scores\")", couleur), "abline(h = 0, col = \"grey\")"),
      legende)),
    list(nom = "loadings", largeur = 6, hauteur = 5, code = if (spectres) {
      c(sprintf("matplot(m$P[, %s], type = \"l\", lty = 1, xlab = %s, ylab = \"p\", main = \"Loadings\")", if (is.na(deux)) "1, drop = FALSE" else sprintf("c(1, %d)", deux),
                code_texte(tx("Terme X", "X term"))), "abline(h = 0, col = \"grey\")")
    } else if (!is.na(deux)) {
      c(sprintf("plot(m$P[, 1], m$P[, %d], type = \"n\", xlab = \"p[1]\", ylab = %s, main = \"Loadings\")", deux, code_texte(nom2[2])),
        "abline(h = 0, v = 0, col = \"grey\")", sprintf("text(m$P[, 1], m$P[, %d], colnames(s$X), cex = 0.7)", deux))
    } else "barplot(m$P[, 1], names.arg = colnames(s$X), las = 2, cex.names = 0.7, main = \"Loadings p[1]\")"),
    if (pls) list(nom = "vip", largeur = 7, hauteur = 4, code = c(
      "ordre <- order(VIP, decreasing = TRUE)",
      "barplot(VIP[ordre], names.arg = colnames(s$X)[ordre], las = 2, cex.names = 0.7, main = \"VIP\")",
      "abline(h = 1, lty = 2, col = \"red\")")),
    if (pls && !pl$plsda) list(nom = "observe-ajuste", largeur = 5, hauteur = 5, code = c(
      if (pl$M > 1) "for (j in seq_len(ncol(Y))) {",
      sprintf("%splot(y_ajuste[, %s], y_observe[, %s], pch = 19, xlab = %s, ylab = %s, main = colnames(s$Y)[%s])", if (pl$M > 1) "  " else "",
              if (pl$M > 1) "j" else "1", if (pl$M > 1) "j" else "1", code_texte(tx("Ajusté", "Fitted")), code_texte(tx("Observé", "Observed")), if (pl$M > 1) "j" else "1"),
      sprintf("%sabline(0, 1, col = \"grey\")", if (pl$M > 1) "  " else ""),
      if (pl$M > 1) "}")),
    list(nom = "t2-dmodx", largeur = if (pl$dmodx) 8 else 5, hauteur = 4, code = c(
      if (pl$dmodx) "op <- par(mfrow = c(1, 2))",
      sprintf("plot(T2, type = \"h\", xlab = \"Observation\", ylab = \"T2\", main = %s)", code_texte(tx("T2 de Hotelling", "Hotelling's T2"))),
      "abline(h = T2Crit, lty = 2, col = c(\"orange\", \"red\"))",
      if (pl$dmodx) c(sprintf("plot(%s, type = \"h\", xlab = \"Observation\", ylab = \"DModX\", main = %s)", pl$dmodx_affiche,
                              code_texte(if (pl$dmodx_affiche == "DModX") "DModX" else tx("DModX pondéré", "Weighted DModX"))),
                      "abline(h = DCrit, lty = 2, col = \"red\")", "par(op)"))))
  Filter(Negate(is.null), graphes)
}

# ---------------------------------------------------------------- les régressions

# Des noms R sûrs pour les colonnes du tableau d'une régression, faits des noms affichés (réponse,
# termes, groupes...) : lettres sans accent, chiffres et _, commençant par une lettre, uniques. Le
# script ne les écrit que dans des chaînes, dont reformulate() fait ses formules : un nom venu des
# données n'y devient jamais du code, et la traduction du script (traduire_code) n'y touche pas.
noms_r <- function(x) {
  x <- chartr("àâäáãåçéèêëíìîïñóòôöõúùûüýÿÀÂÄÁÃÅÇÉÈÊËÍÌÎÏÑÓÒÔÖÕÚÙÛÜÝ", "aaaaaaceeeeiiiinooooouuuuyyAAAAAACEEEEIIIINOOOOOUUUUY", as.character(x))
  for (p in list(c("œ", "oe"), c("Œ", "OE"), c("æ", "ae"), c("Æ", "AE"), c("ß", "ss"))) x <- gsub(p[1], p[2], x, fixed = TRUE)
  x <- gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", x))
  make.unique(make.names(x), sep = "_")
}

# Le plan d'une régression (regression.R : ajuster_regression, cadre_regression) : ses observations
# avant que des valeurs manquantes n'en écartent (le script les écarte comme le moteur), ses termes X
# (plan_bloc, sans pondération), sa réponse, ce qui fait les groupes de ses effets aléatoires, le
# temps de l'autocorrélation et l'effort (le code qui les lit dans `d`), les noms R des colonnes du
# tableau du modèle (noms_r), sa loi, ses courbes et les groupes de sa validation croisée.
plan_regression <- function(projet, d, m, tab) {
  test <- as.numeric(d$test %||% numeric(0))
  candidates <- c(as.numeric(d$observations), test)
  termes <- termes_regression(d$x)
  K <- length(termes)
  generalise <- d$type %in% GENERALISES
  specs <- specs_groupement(d$aleatoires)
  L <- length(specs)
  temps <- if (!is.null(d$correlation)) specs_groupement(d$correlation$temps)[[1]]
  dans <- if (!is.null(d$correlation$dans)) specs_groupement(d$correlation$dans)[[1]]
  gamm <- isTRUE(m$gamm)
  # (un temps fait de dates : le script les lit en années décimales, comme le moteur)
  dates <- !is.null(temps) && isTRUE(attr(temps_en_nombres(valeurs_groupement(projet, temps, candidates)), "dates"))
  # le code qui lit dans `d` les valeurs d'un groupement (des textes), ou le temps (des nombres ;
  # des dates par temps_en_nombres(), une fonction du script)
  lecture <- function(s, nombre = FALSE) {
    if (identical(as.character(s$by)[1], "id")) {
      col <- sprintf("d[[%s]]", code_texte(tab$ids[[as.character(s$id)[1]]]))
      return(if (nombre && dates) sprintf("temps_en_nombres(%s)", col) else if (nombre) sprintf("suppressWarnings(as.numeric(%s))", col) else col)
    }
    v <- variable_du_projet(projet, s$variable)
    col <- sprintf("d[[%s]]", code_texte(tab$noms[[as.character(v$uid)]]))
    if (!length(v$categories)) return(col)
    if (nombre && dates) sprintf("temps_en_nombres(%s)", col)
    else if (nombre) sprintf("suppressWarnings(as.numeric(as.character(%s)))", col) else sprintf("as.character(%s)", col)
  }
  # les noms affichés, puis leurs noms R : réponse, termes, niveaux (et pour mgcv::gam les niveaux
  # emboîtés, un facteur chacun : le secteur dans son site), temps, groupe de l'autocorrélation, effort
  emboites <- if (generalise && !gamm && L >= 2) vapply(2:L, function(l) paste(m$niveaux[seq_len(l)], collapse = "_"), "") else character(0)
  nr <- noms_r(c(m$y$noms, m$noms_termes, m$niveaux, emboites, m$temps, m$dans, m$decalage))
  avant <- 1 + K + L + length(emboites)
  termes_r <- nr[1 + seq_len(K)]
  # les termes en courbe (GAM), dans l'ordre des termes, utiles ou non (le script choisit les utiles)
  lisses <- character(0)
  k_demandes <- integer(0)  # (la dimension demandée d'une courbe, par nom R)
  if (identical(d$type, "GAM") && length(d$lisses)) {
    q <- m$quantitatifs
    en_courbe <- vapply(termes[match(q, m$termes)], function(t) as.numeric(t$variables) %in% as.numeric(d$lisses), TRUE)
    lisses <- termes_r[match(q[en_courbe], m$termes)]
    for (t in q[en_courbe]) {
      k <- d$k_courbes[[as.character(as.numeric(termes[[match(t, m$termes)]]$variables))]]
      if (length(k)) k_demandes[termes_r[match(t, m$termes)]] <- as.integer(k)
    }
  }
  x <- if (K) plan_bloc(projet, termes, m$bloc, tab, candidates, "X")$termes else list()
  ty <- d$y[[1]]
  vy <- variable_du_projet(projet, ty$variables[1])
  groupes <- as.integer(m$vc$groupe)
  G <- as.integer(d$cv_groups %||% 7)
  # la sélection par l'AIC, quand elle a été faite (regression.R : selection_aic) : ses groupes de
  # termes (leurs noms R), ceux que demande chacun, toutes les combinaisons ou l'élimination descendante
  selection <- if (isTRUE(m$selection_aic$K > 0)) {
    # (les groupes qui portent une pente aléatoire restent dans chaque modèle : leurs termes, `gardes`)
    tous <- groupes_modele(m)
    gardes <- as.character(unlist(m$selection_aic$gardes))
    gm <- tous[setdiff(names(tous), gardes)]
    list(toutes = identical(m$selection_aic$methode, "toutes"), limite = m$selection_aic$limite,
         groupes = lapply(gm, function(ts) termes_r[match(ts, m$termes)]), requis = Filter(length, groupes_requis(gm, m$composition)),
         gardes = termes_r[match(unlist(tous[gardes], use.names = FALSE), m$termes)])
  }
  list(nom = as.character(d$name %||% ""), titre = as.character(d$title %||% ""),
       type = "regression", regression = TRUE, modele = as.character(d$type), generalise = generalise, gamm = gamm,
       famille = m$famille, lien = m$lien %||% "identity", plsda = FALSE, opls = FALSE,
       N = m$N, K = K, A = 0, Ap = 0, M = 1, manquantes = FALSE, y_manquantes = FALSE, vides = FALSE,
       rangs = unname(tab$rangs[as.character(d$observations)]), rangs_test = unname(tab$rangs[as.character(test)]),
       condition = plan_condition(projet, d, tab),  # (condition.R)
       noms_tableau = as.character(tab$donnees[[1]]), colonnes = names(tab$donnees),
       x = x, na_termes = K > 0 && anyNA(m$bloc$X),
       croises_tr = any(vapply(x, function(t) t$croise && !is.null(t$tr) && !identite(t$tr), TRUE)),
       y = list(colonne = tab$noms[[as.character(vy$uid)]], nom = m$y$noms, tr = ty$transform,
                garde = !is.null(ty$transform) && hors_domaine(ty$transform, valeurs_simples(projet, ty, candidates))),
       reponse = nr[1], termes = termes_r, noms_termes = m$noms_termes,
       # (les termes qui peuvent entrer dans l'ajustement, d'ordre plus bas d'abord, sans les facteurs
       # seuls ; et les pentes de chaque catégorie sans effet principal : regression.R, candidats_regression
       # et pentes_sans_effet_principal)
       candidats = termes_r[match(m$candidats, m$termes)],
       pentes = termes_r[match(pentes_sans_effet_principal(m$composition, m$candidats), m$termes)],
       # (chaque niveau : sa colonne, son nom affiché, le code qui le lit, et ses pentes aléatoires, les noms R de leurs termes)
       niveaux = lapply(seq_len(L), function(l) list(nom = nr[1 + K + l], affiche = m$niveaux[l], code = lecture(specs[[l]]),
                                                     pentes = termes_r[match(m$pentes[[l]], m$termes)])),
       emboites = nr[1 + K + L + seq_along(emboites)],
       temps = if (!is.null(temps)) list(nom = nr[avant + 1], affiche = m$temps, code = lecture(temps, TRUE), dates = dates),
       dans = if (!is.null(dans)) list(nom = nr[avant + 2], affiche = m$dans, code = lecture(dans)),
       effort = if (!is.null(d$decalage)) list(nom = nr[length(nr)], affiche = m$decalage,
                                               code = sprintf("d[[%s]]", code_texte(tab$noms[[as.character(d$decalage)]]))),
       lisses = lisses, n_courbes = length(m$lisses), k_demandes = k_demandes,
       quantiles = !is.null(m$resultat$residus_quantiles),  # (les résidus quantiles d'un modèle de comptages... : regression_diagnostics.R)
       groupes_vc = groupes, en_tour = identical(groupes, as.integer((seq_along(groupes) - 1) %% G + 1)), G_tour = G,
       # (les groupes de la validation croisée sont-ils les niveaux du premier effet aléatoire, comme par défaut ?)
       vc_premier = L > 0 && length(d$groupes_vc$by) > 0 && as.character(d$groupes_vc$by)[1] %in% c("id", "variable") &&
         identical(specs_groupement(d$groupes_vc)[[1]], specs[[1]][setdiff(names(specs[[1]]), "pentes")]),
       vc_par = as.character(m$vc_par %||% ""), selection = selection,
       # (un modèle choisi par la sélection par AIC : les lignes de d que le modèle complet n'a pas
       # ajustées, faute de valeurs, restent de côté ; regression.R, definition_selection)
       choisi = if (!is.null(d$selection_origine) && length(d$termes_complets))
         list(modele = as.character(d$selection_origine$modele)[1],
              hors = match(setdiff(as.numeric(d$observations), m$observations), as.numeric(d$observations))))
}

# Le type d'une régression, en mots (en-têtes des scripts).
type_regression <- function(pl, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  lois <- c(gaussienne = tx("normale", "normal"), poisson = tx("de Poisson", "Poisson"), binomiale_negative = tx("binomiale négative", "negative binomial"),
            binomiale = tx("binomiale", "binomial"), gamma = "gamma", tweedie = tx("de Tweedie", "Tweedie"))
  mixte <- if (length(pl$niveaux)) tx(" mixte", " mixed") else ""
  if (isTRUE(pl$gamm)) return(sprintf(tx("modèle %s%s, loi normale, avec une autocorrélation CAR1 (mgcv::gamm)", "%s%s model, normal distribution, with a CAR1 autocorrelation (mgcv::gamm)"),
                                      tx(if (identical(pl$modele, "GAM")) "additif généralisé" else "linéaire généralisé", if (identical(pl$modele, "GAM")) "generalized additive" else "generalized linear"), mixte))
  switch(pl$modele,
    LM = tx("régression linéaire", "linear regression"),
    LMM = tx("modèle linéaire mixte", "linear mixed model"),
    GLM = sprintf(tx("modèle linéaire généralisé%s, loi %s", "generalized linear%s model, %s distribution"), mixte, lois[[pl$famille]]),
    sprintf(tx("modèle additif généralisé%s, loi %s", "generalized additive%s model, %s distribution"), mixte, lois[[pl$famille]]))
}

entete_regression <- function(pl, nom_projet, version, fichiers, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  paquet <- c(if (isTRUE(pl$gamm)) c("mgcv", "nlme") else if (pl$generalise) "mgcv" else if (length(pl$niveaux)) "nlme", if (length(pl$lisses)) "MASS")
  c(sprintf(tx("# Modèle %s%s : %s, %d observations, %d termes X", "# Model %s%s: %s, %d observations, %d X terms"),
            texte_sur(pl$nom), if (nzchar(pl$titre)) sprintf(" (%s)", texte_sur(pl$titre)) else "", type_regression(pl, langue), pl$N, pl$K),
    sprintf(tx("# Script R écrit par Nuée %s le %s, à partir du projet « %s ».", "# R script written by Nuée %s on %s, from the project “%s”."), version, format(Sys.Date()), texte_sur(nom_projet)),
    commentaire(sprintf(tx("Il refait ce modèle avec %s, sans Nuée : le calcul est écrit pour ce modèle, de ses données à ses résultats, qui sont ceux de Nuée. Ses quatre parties :",
                           "It redoes this model with %s, without Nuée: the computation is written for this model, from its data to its results, which are those of Nuée. Its four parts:"),
                        texte_paquets(paquet, langue))),
    paste0(tx("#   1. Fonctions : termes utiles, AIC corrigé, validation croisée", "#   1. Functions: useful terms, corrected AIC, cross-validation"),
           if (!is.null(pl$selection)) tx(", sélection par l'AIC", ", AIC selection")),
    tx("#   2. Données", "#   2. Data"),
    paste0(tx("#   3. Modèle : réponse et termes X, ajustement, validation croisée, statistiques", "#   3. Model: response and X terms, fit, cross-validation, statistics"),
           if (!is.null(pl$selection)) tx(", sélection par l'AIC", ", AIC selection")),
    tx("#   4. Résultats : tableaux et graphiques", "#   4. Results: tables and plots"),
    tx("# Dans RStudio, Ctrl+Maj+O affiche ces parties ; Ctrl+Maj+S lance tout le script.", "# In RStudio, Ctrl+Shift+O shows these parts; Ctrl+Shift+S runs the whole script."),
    sprintf(tx("# Écrit avec %s.", "# Written with %s."), texte_paquets(paquet, langue, versions = TRUE)),
    fichiers_a_cote(fichiers, langue, FALSE))
}

# Le code d'une régression (code_regression), exécuté dans `e` et comparé à `m`, celle de
# l'application (ecart_regression).
code_une_regression <- function(pl, m, b, e, langue, seance = NULL) {
  code <- code_regression(pl, b, langue, seance)
  probleme <- evaluer_script(code$calcul, e, avertissements = FALSE)
  list(calcul = code$calcul, sortie = code$sortie, graphiques = code$graphiques, objets = code$objets,
       ecart = if (!is.null(probleme)) probleme else ecart_regression(pl, m, e, langue))
}

# Le code d'une régression : calcul (observations, termes et réponse, tableau du modèle, ajustement,
# validation croisée, statistiques, jeu de test), sortie et graphiques, et les résultats qu'il crée
# (`objets`). Comme regression.R : les termes utiles, les mêmes ajustements (lm ; nlme::lme en REML,
# optim puis nlminb, et en ML ; mgcv::gam en ML sans pénalité, en REML avec, l'AIC d'un ajustement en
# ML), les mêmes groupes de validation croisée, prédits par les effets fixes.
code_regression <- function(pl, b, langue, seance = NULL) {
  tx <- function(fr, en) if (langue == "en") en else fr
  test <- length(pl$rangs_test) > 0
  K <- pl$K
  L <- length(pl$niveaux)
  mgcv <- pl$generalise
  gamm <- isTRUE(pl$gamm)
  mixte <- L > 0
  courbes <- length(pl$lisses) > 0
  car1 <- !is.null(pl$temps)
  loi <- pl$famille %in% c("binomiale_negative", "tweedie")
  poisson <- identical(pl$famille, "poisson")
  quantiles <- isTRUE(pl$quantiles)
  guillemets <- function(x) tx(sprintf("« %s »", texte_sur(x)), sprintf("“%s”", texte_sur(x)))
  liste <- function(x) liste_textes(x, tx("et", "and"))
  affiches <- vapply(pl$niveaux, `[[`, "", "affiche")
  r_niveaux <- vapply(pl$niveaux, `[[`, "", "nom")
  meme_noms <- identical(pl$termes, pl$noms_termes)
  # les effets aléatoires emboîtés, en mots : « par A, puis par B dans chaque A »
  emboitement <- paste(vapply(seq_len(L), function(l) {
    if (l == 1) return(tx(sprintf("par %s", guillemets(affiches[1])), sprintf("for %s", guillemets(affiches[1]))))
    tx(sprintf("puis par %s dans chaque %s", guillemets(affiches[l]), guillemets(affiches[l - 1])),
       sprintf("then for %s within each %s", guillemets(affiches[l]), guillemets(affiches[l - 1])))
  }, ""), collapse = ", ")
  # les noms des termes dans un tableau de coefficients (ses lignes : (Intercept), puis des termes)
  terme_de <- function(lignes) if (meme_noms) lignes else sprintf("c(\"(Intercept)\", noms_termes[match(%s[-1], termes)])", lignes)
  # les colonnes du tableau du modèle qui font les groupes des effets aléatoires dans la formule
  # (mgcv : le premier niveau, puis chaque niveau emboîté)
  re_mgcv <- c(r_niveaux[seq_len(min(L, 1))], pl$emboites)
  # les pentes aléatoires de chaque niveau (les noms R de leurs termes), et en mots : « ; une pente
  # aléatoire de « annee » par « site », corrélée à l'ordonnée à l'origine » (nlme), ou indépendante (mgcv)
  pentes <- lapply(pl$niveaux, function(n) as.character(n$pentes))
  avec_pentes <- length(unlist(pentes)) > 0
  texte_pentes <- if (!avec_pentes) "" else {
    de <- unlist(lapply(seq_len(L), function(l) if (length(pentes[[l]])) {
      noms <- guillemets(pl$noms_termes[match(pentes[[l]], pl$termes)])
      tx(sprintf("de %s par %s", liste(noms), guillemets(affiches[l])), sprintf("of %s for %s", liste(noms), guillemets(affiches[l])))
    }))
    une <- length(unlist(pentes)) == 1
    paste0(tx(if (une) " ; une pente aléatoire " else " ; des pentes aléatoires ", if (une) "; a random slope " else "; random slopes "), liste(de),
           if (mgcv && !gamm) tx(if (une) ", indépendante de l'ordonnée à l'origine (mgcv ne les corrèle pas)" else ", indépendantes des ordonnées à l'origine (mgcv ne les corrèle pas)",
                                 if (une) ", independent of the intercept (mgcv doesn't correlate them)" else ", independent of the intercepts (mgcv doesn't correlate them)")
           else tx(if (une) ", corrélée à l'ordonnée à l'origine de son niveau (la matrice générale de nlme)" else ", corrélées à l'ordonnée à l'origine de leur niveau (la matrice générale de nlme)",
                   if (une) ", correlated with its level's intercept (nlme's general matrix)" else ", correlated with their level's intercept (nlme's general matrix)"))
  }
  # la partie aléatoire de nlme avec les pentes : chaque niveau, ~ annee ou ~ 1 (des chaînes : les noms R ne deviennent pas du code)
  droites_nlme <- vapply(pentes, function(p) paste("~", if (length(p)) paste(p, collapse = " + ") else "1"), "")
  quoi <- c(tx("la réponse", "the response"), if (K) tx("les termes X", "the X terms"),
            if (mixte) tx("les groupes des effets aléatoires", "the groups of the random effects"), if (car1) tx("le temps", "the time"),
            if (!is.null(pl$dans)) tx("le groupe de l'autocorrélation", "the group of the autocorrelation"),
            if (!is.null(pl$effort)) tx("l'effort", "the effort"))
  lectures <- c(vapply(pl$niveaux, `[[`, "", "code"), pl$temps$code, pl$dans$code, pl$effort$code)
  # le groupe de l'autocorrélation : le dernier effet aléatoire, ou un niveau plus fin
  groupes_car1 <- paste(c(r_niveaux, pl$dans$nom), collapse = " / ")
  # (pour les résidus normalisés d'un modèle avec CAR1 : le temps et ce groupe, colonnes du cadre)
  temps_car1 <- if (car1) sprintf("cadre[[%s]]", code_texte(pl$temps$nom))
  groupes_car1_code <- sprintf("interaction(cadre[c(%s)], drop = TRUE)", paste(vapply(c(r_niveaux, pl$dans$nom), code_texte, ""), collapse = ", "))
  dans_affiche <- if (!is.null(pl$dans)) pl$dans$affiche else affiches[L]
  plx <- pl
  plx$manquantes <- pl$na_termes  # (les centres d'un terme croisé sans aucune valeur)
  # les termes qui peuvent entrer dans l'ajustement : tous, dans leur ordre, ou termes_candidats
  # (sans les facteurs seuls, d'ordre plus bas d'abord : regression.R, candidats_regression)
  candidats <- if (identical(pl$candidats, pl$termes)) "termes" else "termes_candidats"
  pentes_sans_effet <- length(pl$pentes) > 0
  # (une catégorie que l'ajustement n'a pas vue, dans une pente sans effet principal : sans prédiction)
  sans_inconnus <- function(prediction, appris, nouvelles) if (!pentes_sans_effet) prediction else sprintf("replace(%s, niveaux_inconnus(%s, %s, pentes_seules), NA)", prediction, appris, nouvelles)

  # --- les observations, les termes, la réponse, le tableau du modèle
  colonnes <- c(
    sprintf("  tab <- data.frame(y[ici]%s)", if (K) ", X[ici, , drop = FALSE]" else ""),
    if (K) "  names(tab) <- c(reponse, termes)" else "  names(tab) <- reponse",
    vapply(pl$niveaux, function(n) sprintf("  tab[[%s]] <- factor(%s[ici])", code_texte(n$nom), n$code), ""),
    vapply(seq_along(pl$emboites), function(l) sprintf("  tab[[%s]] <- interaction(%s, drop = TRUE)", code_texte(pl$emboites[l]),
                                                       paste(sprintf("tab[[%s]]", vapply(r_niveaux[seq_len(l + 1)], code_texte, "")), collapse = ", ")), ""),
    if (car1) sprintf("  tab[[%s]] <- %s[ici]", code_texte(pl$temps$nom), pl$temps$code),
    if (!is.null(pl$dans)) sprintf("  tab[[%s]] <- factor(%s[ici])", code_texte(pl$dans$nom), pl$dans$code),
    if (!is.null(pl$effort)) sprintf("  tab[[%s]] <- %s[ici]", code_texte(pl$effort$nom), pl$effort$code),
    "  tab")
  filtre <- function(cadre, noms) if (pl$croises_tr) c(
    sprintf("retenues <- complete.cases(%s)  # %s", cadre, tx("un terme croisé hors du domaine de sa transformation : manquant", "a cross term outside the domain of its transformation: missing")),
    sprintf("%s <- %s[retenues]", noms, noms), sprintf("%s <- %s[retenues, , drop = FALSE]", cadre, cadre))
  donnees <- c(
    code_observations(pl, tx), "",
    if (K) c(code_termes(pl$x, "X", pl, tx), ""),
    commentaire(tx(sprintf("La réponse : %s, une colonne du tableau.", guillemets(pl$y$nom)), sprintf("The response: %s, a column of the table.", guillemets(pl$y$nom)))),
    sprintf("y <- d[[%s]]", code_texte(pl$y$colonne)),
    if (!is.null(pl$y$tr) && !identite(pl$y$tr)) sprintf("y <- %s  # %s", code_transformation(pl$y$tr, "y", pl$y$garde), nom_transformation_script(pl$y$tr, pl$y$garde, tx)),
    "",
    commentaire(tx(sprintf("Les lignes où %s sont connus (`connu`). Celles du modèle (`apprentissage`) font les centres et poids des termes, puis le tableau du modèle%s.",
                           liste(quoi), if (test) " ; les autres, celui du jeu de test" else ""),
                   sprintf("The rows where %s are known (`known`). Those of the model (`training`) give the centres and weights of the terms, then the table of the model%s.",
                           liste(quoi), if (test) "; the others, that of the test set" else ""))),
    sprintf("connu <- complete.cases(%s)", paste(c("y", if (K) "X", lectures), collapse = ", ")),
    if (length(pl$choisi$hors)) c(
      commentaire(tx(sprintf("Ce modèle est celui que la sélection par l'AICc du modèle %s a choisi : il est ajusté sur les lignes de %s, où la sélection l'a comparé aux autres ; les %d lignes que %s n'a pas ajustées, faute de valeurs, restent de côté.",
                             guillemets(pl$choisi$modele), guillemets(pl$choisi$modele), length(pl$choisi$hors), guillemets(pl$choisi$modele)),
                     sprintf("This model is the one the AICc selection of model %s chose: it is fitted on the rows of %s, where the selection compared it with the others; the %d rows %s did not fit, for missing values, stay out.",
                             guillemets(pl$choisi$modele), guillemets(pl$choisi$modele), length(pl$choisi$hors), guillemets(pl$choisi$modele)))),
      code_valeurs(morceaux_rangs(pl$choisi$hors), "hors_selection <- "),
      "connu[hors_selection] <- FALSE"),
    if (test) "apprentissage <- intersect(which(connu), modele)" else "apprentissage <- which(connu)",
    if (K) c("", code_echelle_bloc(pl$x, "X", "x", plx, tx, "X")),
    "",
    commentaire(tx(sprintf("Le tableau du modèle pour les lignes `ici` de d : la réponse, %s%s%s%s. Ses colonnes ont des noms R (`reponse`, `termes`%s).",
                           if (K) "les termes X" else "sans terme X",
                           if (mixte) sprintf(", les groupes de chaque effet aléatoire en facteurs%s", if (length(pl$emboites)) " (mgcv : un facteur par niveau emboîté, chacun dans le précédent)" else "") else "",
                           if (car1) ", le temps" else "", if (!is.null(pl$effort)) ", l'effort" else "",
                           if (!meme_noms) " ; `noms_termes` : les noms des termes dans Nuée" else ""),
                   sprintf("The table of the model for the rows `here` of d: the response, %s%s%s%s. Its columns have R names (`response`, `model_terms`%s).",
                           if (K) "the X terms" else "no X term",
                           if (mixte) sprintf(", the groups of each random effect as factors%s", if (length(pl$emboites)) " (mgcv: one factor per nested level, each within the previous one)" else "") else "",
                           if (car1) ", the time" else "", if (!is.null(pl$effort)) ", the effort" else "",
                           if (!meme_noms) "; `term_labels`: the names of the terms in Nuée" else ""))),
    sprintf("reponse <- %s", code_texte(pl$reponse)),
    if (K) code_valeurs(vapply(pl$termes, code_texte, ""), "termes <- ") else "termes <- character(0)",
    if (!meme_noms) code_valeurs(vapply(pl$noms_termes, code_texte, ""), "noms_termes <- "),
    if (candidats != "termes") c(
      commentaire(tx("Les termes qui peuvent entrer dans l'ajustement, dans l'ordre où termes_utiles() les prend : sans ceux des variables qui n'ont pas d'effet principal (ils ne servent qu'à calculer les termes croisés), les termes d'ordre plus bas d'abord, comme R les range : la catégorie de référence d'une variable qualitative, et les colonnes d'un terme croisé qui se déduisent de ses effets principaux, sont laissées de côté.",
                     "The terms that can enter the fit, in the order useful_terms() takes them: without those of the variables that have no main effect (they only serve to compute the cross terms), the lower-order terms first, as R orders them: the reference category of a qualitative variable, and the columns of a cross term that follow from its main effects, are left out.")),
      code_valeurs(vapply(pl$candidats, code_texte, ""), "termes_candidats <- ")),
    if (pentes_sans_effet) c(
      commentaire(tx("La pente de chaque catégorie, sans l'effet principal de sa variable quantitative (niveaux_inconnus() ci-dessus).",
                     "Each category's slope, without the main effect of its quantitative variable (unseen_levels() above).")),
      code_valeurs(vapply(pl$pentes, code_texte, ""), "pentes_seules <- ")),
    "tableau_modele <- function(ici) {", colonnes, "}",
    "cadre <- tableau_modele(apprentissage)",
    "noms_obs <- d[[\"Observation\"]][apprentissage]",
    filtre("cadre", "noms_obs"),
    if (test) c("cadre_test <- tableau_modele(setdiff(which(connu), modele))",
                "noms_test <- d[[\"Observation\"]][setdiff(which(connu), modele)]",
                filtre("cadre_test", "noms_test")))

  # --- l'ajustement
  formule <- function(droite) c(sprintf("  droite <- %s", droite),
                                "  f <- reformulate(if (length(droite)) droite else \"1\", response = reponse)")
  ajustement <- if (gamm) {
    droite_gamm <- paste(c(if (courbes) "setdiff(utiles, lis)" else "utiles", if (courbes) "sprintf(\"s(%s, k = %d)\", lis, kk)"), collapse = ", ")
    c(commentaire(tx("La loi de la réponse : normale, lien identité (avec une autocorrélation, mgcv::gamm ne prend qu'elle : une autre loi passerait par une quasi-vraisemblance, sans AIC).",
                     "The distribution of the response: normal, identity link (with an autocorrelation, mgcv::gamm takes only that one: another would go through a quasi-likelihood, with no AIC).")),
      if (courbes) c(commentaire(tx("Les termes en courbe (des splines de régression en plaques minces) et la dimension de base de chacune.",
                                    "The terms as curves (thin plate regression splines) and the basis dimension of each.")),
                     code_valeurs(vapply(pl$lisses, code_texte, ""), "lisses <- "),
                     if (length(pl$k_demandes)) sprintf("k_demandes <- c(%s)", paste(sprintf("%s = %dL", vapply(names(pl$k_demandes), code_texte, ""), pl$k_demandes), collapse = ", ")),
                     sprintf("k <- vapply(lisses, function(t) dimension_courbe(cadre[[t]]%s), 0L)", if (length(pl$k_demandes)) ", k_demandes[t]" else "")),
      commentaire(tx(sprintf("Les effets aléatoires (de nlme) : une ordonnée à l'origine aléatoire %s%s ; une autocorrélation des résidus en temps continu (CAR1) le long de %s, dans chaque %s.",
                             emboitement, texte_pentes, guillemets(pl$temps$affiche), guillemets(dans_affiche)),
                     sprintf("The random effects (of nlme): a random intercept %s%s; a continuous-time autocorrelation of the residuals (CAR1) along %s, within each %s.",
                             emboitement, texte_pentes, guillemets(pl$temps$affiche), guillemets(dans_affiche)))),
      code_valeurs(vapply(r_niveaux, code_texte, ""), "niveaux_aleatoires <- "),
      if (avec_pentes) sprintf("aleatoire <- setNames(lapply(%s, as.formula), niveaux_aleatoires)", code_textes(droites_nlme))
      else "aleatoire <- setNames(rep(list(~ 1), length(niveaux_aleatoires)), niveaux_aleatoires)",
      sprintf("correlation <- nlme::corCAR1(form = as.formula(%s))", code_texte(paste("~", pl$temps$nom, "|", groupes_car1))),
      commentaire(tx("Ajustement : un modèle additif mixte (mgcv::gamm), ajusté comme un modèle linéaire mixte de nlme dont les courbes sont des effets aléatoires, avec les termes utiles sur ces lignes (termes_utiles()) ; REML pour les coefficients, les courbes, les variances et l'autocorrélation, ML (methode = \"ML\") pour la vraisemblance et l'AIC, ceux de sa partie nlme (m$lme). Un second essai passe par optim.",
                     "Fit: a mixed additive model (mgcv::gamm), fitted as a linear mixed model of nlme whose curves are random effects, with the useful terms on these rows (useful_terms()); REML for the coefficients, the curves, the variances and the autocorrelation, ML (method = \"ML\") for the likelihood and the AIC, those of its nlme part (m$lme). A second attempt goes through optim.")),
      sprintf("ajuster_modele <- function(tab, candidats = %s, methode = \"REML\") {", candidats),
      "  utiles <- termes_utiles(tab, candidats)",
      if (courbes) c("  lis <- intersect(lisses, utiles)",
                     "  kk <- vapply(lis, function(t) dimension_courbe(tab[[t]], k[[t]]), 0L)",
                     "  lis <- lis[kk >= 3]",
                     "  kk <- kk[kk >= 3]"),
      formule(sprintf("c(%s)", droite_gamm)),
      "  tenter <- function(...) tryCatch(gamm(f, random = aleatoire, correlation = correlation, data = tab, method = methode, ...), error = function(e) NULL)",
      "  m <- tenter()",
      "  if (is.null(m)) m <- tenter(control = nlme::lmeControl(opt = \"optim\", maxIter = 200, msMaxIter = 200, returnObject = TRUE))",
      "  m",
      "}",
      "m <- ajuster_modele(cadre)",
      "m_ml <- ajuster_modele(cadre, methode = \"ML\")",
      sprintf("utiles <- termes_utiles(cadre, %s)", candidats))
  } else if (mgcv) {
    familles <- c(gaussienne = "gaussian()", poisson = "poisson()", binomiale = "binomial()", binomiale_negative = "nb()",
                  gamma = "Gamma(link = \"log\")", tweedie = "tw()")
    noms_lois <- c(gaussienne = tx("normale, lien identité", "normal, identity link"), poisson = tx("de Poisson, lien log", "Poisson, log link"),
                   binomiale = tx("binomiale, lien logit", "binomial, logit link"), binomiale_negative = tx("binomiale négative, lien log (son θ estimé)", "negative binomial, log link (its θ estimated)"),
                   gamma = tx("gamma, lien log", "gamma, log link"), tweedie = tx("de Tweedie, lien log (sa puissance p estimée)", "Tweedie, log link (its power p estimated)"))
    penalise <- courbes || mixte
    droite <- paste(c(if (courbes) "setdiff(utiles, lis)" else "utiles",
                      if (courbes) "sprintf(\"s(%s, k = %d)\", lis, kk)",
                      if (mixte) "sprintf(\"s(%s, bs = \\\"re\\\")\", aleatoires)",
                      if (avec_pentes) "sprintf(\"s(%s, %s, bs = \\\"re\\\")\", names(pentes_aleatoires), pentes_aleatoires)",
                      if (!is.null(pl$effort)) code_texte(sprintf("offset(log(%s))", pl$effort$nom))), collapse = ", ")
    # (les pentes aléatoires : le facteur de leur niveau, en nom, et leur terme)
    pentes_mgcv <- unlist(lapply(seq_len(L), function(l) stats::setNames(pentes[[l]], rep(re_mgcv[l], length(pentes[[l]])))))
    c(commentaire(tx(sprintf("La loi de la réponse : %s. Une loi neuve pour chaque ajustement (famille()) : celles de mgcv gardent le paramètre que leur dernier ajustement a estimé.", noms_lois[[pl$famille]]),
                     sprintf("The distribution of the response: %s. A new one for each fit (distribution()): those of mgcv keep the parameter their last fit estimated.", noms_lois[[pl$famille]]))),
      sprintf("famille <- function() %s", familles[[pl$famille]]),
      if (courbes) c(commentaire(tx("Les termes en courbe (des splines de régression en plaques minces) et la dimension de base de chacune.",
                                    "The terms as curves (thin plate regression splines) and the basis dimension of each.")),
                     code_valeurs(vapply(pl$lisses, code_texte, ""), "lisses <- "),
                     if (length(pl$k_demandes)) sprintf("k_demandes <- c(%s)", paste(sprintf("%s = %dL", vapply(names(pl$k_demandes), code_texte, ""), pl$k_demandes), collapse = ", ")),
                     sprintf("k <- vapply(lisses, function(t) dimension_courbe(cadre[[t]]%s), 0L)", if (length(pl$k_demandes)) ", k_demandes[t]" else "")),
      if (mixte) c(commentaire(tx(sprintf("Les effets aléatoires (des termes pénalisés) : une ordonnée à l'origine aléatoire %s%s%s.", emboitement,
                                          if (length(pl$emboites)) " (mgcv : un facteur par niveau emboîté)" else "", texte_pentes),
                                  sprintf("The random effects (penalized terms): a random intercept %s%s%s.", emboitement,
                                          if (length(pl$emboites)) " (mgcv: one factor per nested level)" else "", texte_pentes))),
                   code_valeurs(vapply(re_mgcv, code_texte, ""), "aleatoires <- "),
                   if (avec_pentes) sprintf("pentes_aleatoires <- setNames(%s, %s)  # %s", code_textes(pentes_mgcv), code_textes(names(pentes_mgcv)),
                                            tx("le terme de chaque pente aléatoire, nommé par le facteur de son niveau", "each random slope's term, named by its level's factor"))),
      commentaire(tx(sprintf("Ajustement : un modèle %s (mgcv::gam) avec les termes utiles sur ces lignes (termes_utiles())%s%s ; %s.",
                             if (identical(pl$modele, "GAM")) "additif généralisé" else "linéaire généralisé",
                             if (courbes) ", une courbe pour chaque terme de `lisses` (sa dimension réduite si ces lignes ont trop peu de valeurs ; en dessous de 3, il reste une droite)" else "",
                             if (!is.null(pl$effort)) sprintf(", et log(%s) en décalage (l'effort)", guillemets(pl$effort$affiche)) else "",
                             if (penalise) "REML pour les courbes et les effets aléatoires (methode = \"ML\" : l'ajustement dont viennent la vraisemblance et l'AIC)" else "en maximum de vraisemblance (sans terme pénalisé)"),
                     sprintf("Fit: a %s model (mgcv::gam) with the useful terms on these rows (useful_terms())%s%s; %s.",
                             if (identical(pl$modele, "GAM")) "generalized additive" else "generalized linear",
                             if (courbes) ", a curve for each term of `smooth_terms` (its dimension reduced if these rows have too few values; below 3, it stays a straight line)" else "",
                             if (!is.null(pl$effort)) sprintf(", and log(%s) as an offset (the effort)", guillemets(pl$effort$affiche)) else "",
                             if (penalise) "REML for the curves and the random effects (method = \"ML\": the fit that gives the likelihood and the AIC)" else "by maximum likelihood (no penalized term)"))),
      sprintf("ajuster_modele <- function(tab, candidats = %s, methode = %s) {", candidats, if (mixte) "\"REML\"" else if (penalise) "NULL" else "\"ML\""),
      "  utiles <- termes_utiles(tab, candidats)",
      if (courbes) c("  lis <- intersect(lisses, utiles)",
                     "  kk <- vapply(lis, function(t) dimension_courbe(tab[[t]], k[[t]]), 0L)",
                     "  lis <- lis[kk >= 3]",
                     "  kk <- kk[kk >= 3]"),
      formule(sprintf("c(%s)", droite)),
      if (!mixte && penalise) "  if (is.null(methode)) methode <- if (length(lis)) \"REML\" else \"ML\"",
      "  tryCatch(gam(f, data = tab, family = famille(), method = methode), error = function(e) NULL)",
      "}",
      "m <- ajuster_modele(cadre)",
      sprintf("utiles <- termes_utiles(cadre, %s)", candidats),
      if (!penalise) tx("m_ml <- m  # (sans terme pénalisé : un seul ajustement, en ML)", "m_ml <- m  # (no penalized term: a single fit, by ML)")
      else if (mixte) "m_ml <- ajuster_modele(cadre, methode = \"ML\")"
      else "m_ml <- if (length(intersect(lisses, utiles))) ajuster_modele(cadre, methode = \"ML\") else m")
  } else if (mixte) {
    niveaux_txt <- paste(r_niveaux, collapse = " / ")
    c(commentaire(tx(sprintf("Les effets aléatoires : une ordonnée à l'origine aléatoire %s%s%s.", emboitement, texte_pentes,
                             if (car1) sprintf(" ; une autocorrélation des résidus en temps continu (CAR1) le long de %s, dans chaque %s", guillemets(pl$temps$affiche), guillemets(dans_affiche)) else ""),
                     sprintf("The random effects: a random intercept %s%s%s.", emboitement, texte_pentes,
                             if (car1) sprintf("; a continuous-time autocorrelation of the residuals (CAR1) along %s, within each %s", guillemets(pl$temps$affiche), guillemets(dans_affiche)) else ""))),
      # (avec des pentes : ~ annee | site pour un niveau, sinon la liste de nlme, chaque niveau dans le précédent)
      if (!avec_pentes) sprintf("aleatoire <- as.formula(%s)", code_texte(paste("~ 1 |", niveaux_txt)))
      else if (L == 1) sprintf("aleatoire <- as.formula(%s)", code_texte(paste(droites_nlme, "|", r_niveaux)))
      else c(code_valeurs(vapply(r_niveaux, code_texte, ""), "niveaux_aleatoires <- "),
             sprintf("aleatoire <- setNames(lapply(%s, as.formula), niveaux_aleatoires)  # %s", code_textes(droites_nlme),
                     tx("les niveaux emboîtés, du plus large au plus fin", "the nested levels, the widest first"))),
      if (car1) sprintf("correlation <- nlme::corCAR1(form = as.formula(%s))", code_texte(paste("~", pl$temps$nom, "|", groupes_car1))),
      commentaire(tx("Ajustement : un modèle linéaire mixte (nlme::lme) avec les termes utiles sur ces lignes (termes_utiles()) ; REML pour les coefficients et les variances, ML (methode = \"ML\") pour la vraisemblance et l'AIC. L'optimisation essaie optim, puis nlminb.",
                     "Fit: a linear mixed model (nlme::lme) with the useful terms on these rows (useful_terms()); REML for the coefficients and the variances, ML (method = \"ML\") for the likelihood and the AIC. The optimisation tries optim, then nlminb.")),
      sprintf("ajuster_modele <- function(tab, candidats = %s, methode = \"REML\") {", candidats),
      "  utiles <- termes_utiles(tab, candidats)",
      "  f <- reformulate(if (length(utiles)) utiles else \"1\", response = reponse)",
      sprintf("  tenter <- function(reglages) tryCatch(nlme::lme(f, random = aleatoire, %sdata = tab, method = methode, control = reglages),", if (car1) "correlation = correlation, " else ""),
      "                                        error = function(e) NULL)",
      "  m <- tenter(nlme::lmeControl(opt = \"optim\", maxIter = 200, msMaxIter = 200, returnObject = TRUE))",
      "  if (is.null(m)) m <- tenter(nlme::lmeControl(maxIter = 500, msMaxIter = 500, returnObject = TRUE))",
      sprintf("  if (!is.null(m)) m$call$fixed <- f  # %s", tx("(la formule elle-même dans l'appel : predict() de nlme 3.1-164 et d'avant la relit)",
                                                              "(the formula itself in the call: predict() of nlme 3.1-164 and before reads it again)")),
      "  m",
      "}",
      "m <- ajuster_modele(cadre)",
      "m_ml <- ajuster_modele(cadre, methode = \"ML\")",
      sprintf("utiles <- termes_utiles(cadre, %s)", candidats))
  } else {
    c(commentaire(tx("Ajustement : une régression linéaire par les moindres carrés ordinaires (lm), avec les termes utiles sur ces lignes (termes_utiles()).",
                     "Fit: a linear regression by ordinary least squares (lm), with the useful terms on these rows (useful_terms()).")),
      sprintf("ajuster_modele <- function(tab, candidats = %s) {", candidats),
      "  utiles <- termes_utiles(tab, candidats)",
      "  tryCatch(lm(reformulate(if (length(utiles)) utiles else \"1\", response = reponse), data = tab), error = function(e) NULL)",
      "}",
      "m <- ajuster_modele(cadre)",
      tx("m_ml <- m  # (moindres carrés : la vraisemblance est celle de cet ajustement)", "m_ml <- m  # (least squares: the likelihood is that of this fit)"),
      sprintf("utiles <- termes_utiles(cadre, %s)", candidats))
  }

  # --- la validation croisée
  par <- if (nzchar(pl$vc_par)) tx(sprintf(", un par valeur de %s", guillemets(pl$vc_par)), sprintf(", one per value of %s", guillemets(pl$vc_par)))
         else tx(" (les observations à tour de rôle)", " (the observations in turn)")
  predire <- paste("function(m, nouvelles, appris)", sans_inconnus(
    if (gamm) "tryCatch(as.vector(predict(m$gam, nouvelles, type = \"response\")), error = function(e) NA_real_)"
    else if (mgcv) {
      if (mixte) "tryCatch(predire_fixes(m, nouvelles, appris, aleatoires), error = function(e) NA_real_)"
      else "tryCatch(as.vector(predict(m, nouvelles, type = \"response\")), error = function(e) NA_real_)"
    } else if (mixte) "predict(m, nouvelles, level = 0)" else "predict(m, nouvelles)", "appris", "nouvelles"))
  validation <- c(
    commentaire(tx(sprintf("Validation croisée en %d groupes%s : chaque groupe prédit par le modèle ajusté sans lui, avec ses termes utiles parmi ceux du modèle%s.",
                           length(unique(pl$groupes_vc)), par, if (mixte) ", par ses seuls effets fixes (comme un groupe jamais vu)" else ""),
                   sprintf("Cross-validation in %d groups%s: each group predicted by the model fitted without it, with its useful terms among those of the model%s.",
                           length(unique(pl$groupes_vc)), par, if (mixte) ", by its fixed effects only (as a group never seen)" else ""))),
    if (pl$en_tour) sprintf("groupes_vc <- (seq_len(nrow(cadre)) - 1) %%%% %d + 1", pl$G_tour)
    else if (pl$vc_premier) sprintf("groupes_vc <- as.integer(cadre[[%s]])  # %s", code_texte(r_niveaux[1]), tx("ses niveaux", "its levels"))
    else code_groupes_vc(pl$groupes_vc),
    "vc <- validation_croisee(cadre, groupes_vc, function(tab) ajuster_modele(tab, utiles),",
    sprintf("                        %s)", predire))

  # --- les statistiques
  ic <- tx("Les coefficients : estimation, erreur-type, degrés de liberté, t, p et intervalle de confiance à 95 %.",
           "The coefficients: estimate, standard error, degrees of freedom, t, p and 95% confidence interval.")
  statistiques <- if (gamm) c(
    commentaire(ic),
    "g <- m$gam",
    "sm <- summary(g)",
    "pt <- sm$p.table",
    "q <- qt(0.975, g$df.residual)",
    sprintf("coefficients <- data.frame(terme = %s, estimation = pt[, 1], erreur_type = pt[, 2], ddl = g$df.residual, t = pt[, 3], p = pt[, 4],", terme_de("rownames(pt)")),
    "                           bas = pt[, 1] - q * pt[, 2], haut = pt[, 1] + q * pt[, 2], row.names = NULL)",
    if (courbes) c(commentaire(tx("Les courbes : leurs degrés de liberté effectifs (edf ; 1 est une droite) et leur test.", "The curves: their effective degrees of freedom (edf; 1 is a straight line) and their test.")),
                   "lis <- intersect(lisses, utiles)",
                   "st <- sm$s.table[sprintf(\"s(%s)\", lis), , drop = FALSE]",
                   sprintf("courbes <- data.frame(terme = %s, edf = st[, 1], ref_ddl = st[, 2], statistique = st[, 3], p = st[, 4], row.names = NULL)",
                           if (meme_noms) "lis" else "noms_termes[match(lis, termes)]"),
                   tx("# la concurvité : la part de chaque courbe que les autres termes reproduisent (au-delà de 0,8, elle se confond avec eux)",
                      "# the concurvity: the share of each curve the other terms reproduce (above 0.8, it is confounded with them)"),
                   "cc <- concurvity(g, full = TRUE)",
                   "courbes$concurvite <- unname(cc[\"estimate\", sprintf(\"s(%s)\", lis)])",
                   "courbes$concurvite_pire <- unname(cc[\"worst\", sprintf(\"s(%s)\", lis)])",
                   tx("# les seuils de chaque courbe (seuils_courbe ci-dessus), et la liste de leurs plages nettes",
                      "# the thresholds of each curve (curve_thresholds above), and the list of their clear ranges"),
                   "seuils <- lapply(lis, function(courbe) seuils_courbe(g, courbe, cadre, centre_x[match(courbe, termes)], poids_x[match(courbe, termes)]))",
                   "names(seuils) <- courbes$terme",
                   "plages_seuils <- do.call(rbind, lapply(names(seuils), function(n) data.frame(terme = rep(n, nrow(seuils[[n]]$plages)), seuils[[n]]$plages, row.names = NULL)))"),
    commentaire(tx("Les variances (REML) : celle de chaque effet aléatoire (σ² fois sa matrice relative dans la partie nlme), puis la résiduelle ; le R² marginal et conditionnel ; l'autocorrélation φ.",
                   "The variances (REML): that of each random effect (σ² times its relative matrix in the nlme part), then the residual one; the marginal and conditional R²; the autocorrelation φ.")),
    if (avec_pentes) sprintf("variances <- variances_pentes(m$lme, niveaux_aleatoires, %s, %s)", code_textes(affiches), code_texte(tx("résiduelle", "residual")))
    else sprintf("variances <- data.frame(composante = %s, variance = c(vapply(niveaux_aleatoires, function(n) m$lme$sigma^2 * as.matrix(m$lme$modelStruct$reStruct[[n]])[1, 1], 0), m$lme$sigma^2), row.names = NULL)",
                 code_textes(c(affiches, tx("résiduelle", "residual")))),
    "r2 <- r2_gamm(m, niveaux_aleatoires, cadre)",
    sprintf("phi <- unname(coef(m$lme$modelStruct$corStruct, unconstrained = FALSE))  # %s", tx("l'autocorrélation CAR1", "the CAR1 autocorrelation"))
  ) else if (mgcv) c(
    commentaire(c(ic, if (identical(pl$lien, "log")) tx("Avec le lien log, exp(estimation) est l'effet multiplicatif d'une unité de plus.", "With the log link, exp(estimate) is the multiplicative effect of one more unit.")
                      else if (identical(pl$lien, "logit")) tx("Avec le lien logit, exp(estimation) est le rapport des cotes.", "With the logit link, exp(estimate) is the odds ratio."))),
    "sm <- summary(m)",
    "pt <- sm$p.table",
    "echelle_estimee <- isTRUE(m$scale.estimated)",
    "q <- if (echelle_estimee) qt(0.975, m$df.residual) else qnorm(0.975)",
    sprintf("coefficients <- data.frame(terme = %s, estimation = pt[, 1], erreur_type = pt[, 2],", terme_de("rownames(pt)")),
    "                           ddl = if (echelle_estimee) m$df.residual else NA_real_, t = pt[, 3], p = pt[, 4],",
    "                           bas = pt[, 1] - q * pt[, 2], haut = pt[, 1] + q * pt[, 2], row.names = NULL)",
    if (identical(pl$lien, "log")) "coefficients$effet_multiplicatif <- exp(coefficients$estimation)"
    else if (identical(pl$lien, "logit")) "coefficients$rapport_cotes <- exp(coefficients$estimation)",
    if (courbes) c(commentaire(tx("Les courbes : leurs degrés de liberté effectifs (edf ; 1 est une droite) et leur test.", "The curves: their effective degrees of freedom (edf; 1 is a straight line) and their test.")),
                   "lis <- intersect(lisses, utiles)",
                   "st <- sm$s.table[sprintf(\"s(%s)\", lis), , drop = FALSE]",
                   sprintf("courbes <- data.frame(terme = %s, edf = st[, 1], ref_ddl = st[, 2], statistique = st[, 3], p = st[, 4], row.names = NULL)",
                           if (meme_noms) "lis" else "noms_termes[match(lis, termes)]"),
                   tx("# la concurvité : la part de chaque courbe que les autres termes reproduisent (au-delà de 0,8, elle se confond avec eux)",
                      "# the concurvity: the share of each curve the other terms reproduce (above 0.8, it is confounded with them)"),
                   "cc <- concurvity(m, full = TRUE)",
                   "courbes$concurvite <- unname(cc[\"estimate\", sprintf(\"s(%s)\", lis)])",
                   "courbes$concurvite_pire <- unname(cc[\"worst\", sprintf(\"s(%s)\", lis)])",
                   tx("# les seuils de chaque courbe (seuils_courbe ci-dessus), et la liste de leurs plages nettes",
                      "# the thresholds of each curve (curve_thresholds above), and the list of their clear ranges"),
                   "seuils <- lapply(lis, function(courbe) seuils_courbe(m, courbe, cadre, centre_x[match(courbe, termes)], poids_x[match(courbe, termes)]))",
                   "names(seuils) <- courbes$terme",
                   "plages_seuils <- do.call(rbind, lapply(names(seuils), function(n) data.frame(terme = rep(n, nrow(seuils[[n]]$plages)), seuils[[n]]$plages, row.names = NULL)))"),
    if (mixte) c(commentaire(tx("La variance de chaque effet aléatoire (gam.vcomp : ses écarts-types).", "The variance of each random effect (gam.vcomp: their standard deviations).")),
                 "vcomp <- tryCatch(gam.vcomp(m), error = function(e) NULL)",
                 "ecart_types <- if (is.matrix(vcomp)) vcomp[, 1] else if (is.list(vcomp) && is.matrix(vcomp$vc)) vcomp$vc[, 1]",
                 if (!avec_pentes) sprintf("variances <- if (!is.null(ecart_types)) data.frame(composante = %s, variance = unname(ecart_types[sprintf(\"s(%%s)\", aleatoires)])^2)",
                                           code_textes(affiches))
                 else {
                   # (chaque niveau : son ordonnée à l'origine, puis ses pentes, comme dans Nuée)
                   lignes <- unlist(lapply(seq_len(L), function(l) c(sprintf("s(%s)", re_mgcv[l]), sprintf("s(%s,%s)", re_mgcv[l], pentes[[l]]))))
                   niveaux_lignes <- rep(affiches, 1 + lengths(pentes))
                   termes_lignes <- unlist(lapply(pentes, function(p) c("(Intercept)", p)))
                   c(sprintf("variances <- if (!is.null(ecart_types)) data.frame(composante = %s, terme = %s,", code_textes(niveaux_lignes), code_textes(termes_lignes)),
                     sprintf("                                                variance = unname(ecart_types[%s])^2)", code_textes(lignes)))
                 }),
    commentaire(tx("La part de la variation expliquée : R² ajusté et part de la déviance.", "The share of the variation explained: adjusted R² and share of the deviance.")),
    "r2 <- c(r2_ajuste = sm$r.sq, deviance_expliquee = sm$dev.expl)",
    if (loi) sprintf("parametre_loi <- c(%s = m$family$getTheta(TRUE))  # %s", if (identical(pl$famille, "tweedie")) "p" else "theta",
                     if (identical(pl$famille, "tweedie")) tx("la puissance de la loi de Tweedie", "the power of the Tweedie distribution") else tx("le θ de la binomiale négative", "the θ of the negative binomial")),
    if (poisson) sprintf("dispersion <- sum(residuals(m, type = \"pearson\")^2) / m$df.residual  # %s",
                         tx("khi² de Pearson / ddl : au-delà de 1,5, des comptages surdispersés", "Pearson's chi-square / df: above 1.5, overdispersed counts"))
  ) else if (mixte) c(
    commentaire(ic),
    "tt <- summary(m)$tTable",
    "q <- qt(0.975, tt[, \"DF\"])",
    sprintf("coefficients <- data.frame(terme = %s, estimation = tt[, \"Value\"], erreur_type = tt[, \"Std.Error\"], ddl = tt[, \"DF\"],", terme_de("rownames(tt)")),
    "                           t = tt[, \"t-value\"], p = tt[, \"p-value\"], bas = tt[, \"Value\"] - q * tt[, \"Std.Error\"],",
    "                           haut = tt[, \"Value\"] + q * tt[, \"Std.Error\"], row.names = NULL)",
    commentaire(tx("Les variances (REML) : celle de chaque effet aléatoire (σ² fois sa matrice relative), puis la résiduelle ; le R² marginal et conditionnel.",
                   "The variances (REML): that of each random effect (σ² times its relative matrix), then the residual one; the marginal and conditional R².")),
    if (avec_pentes) sprintf("variances <- variances_pentes(m, %s, %s, %s)", code_textes(r_niveaux), code_textes(affiches), code_texte(tx("résiduelle", "residual")))
    else sprintf("variances <- data.frame(composante = %s, variance = c(vapply(%s, function(n) m$sigma^2 * as.matrix(m$modelStruct$reStruct[[n]])[1, 1], 0), m$sigma^2), row.names = NULL)",
                 code_textes(c(affiches, tx("résiduelle", "residual"))), code_textes(r_niveaux)),
    "r2 <- r2_mixte(m)",
    if (car1) sprintf("phi <- unname(coef(m$modelStruct$corStruct, unconstrained = FALSE))  # %s", tx("l'autocorrélation CAR1", "the CAR1 autocorrelation"))
  ) else c(
    commentaire(ic),
    "tt <- summary(m)$coefficients",
    "ic <- confint(m)",
    sprintf("coefficients <- data.frame(terme = %s, estimation = tt[, 1], erreur_type = tt[, 2], ddl = df.residual(m), t = tt[, 3],", terme_de("rownames(tt)")),
    "                           p = tt[, 4], bas = ic[, 1], haut = ic[, 2], row.names = NULL)",
    "r2 <- c(r2 = summary(m)$r.squared, r2_ajuste = summary(m)$adj.r.squared)")
  statistiques <- c(statistiques, "",
    commentaire(tx("La vraisemblance (de l'ajustement en ML), son nombre de paramètres, l'AIC, l'AICc et le BIC.",
                   "The likelihood (of the ML fit), its number of parameters, the AIC, the AICc and the BIC.")),
    if (gamm) "ll <- logLik(m_ml$lme)" else "ll <- logLik(m_ml)",
    if (gamm) "criteres <- c(logLik = as.numeric(ll), parametres = attr(ll, \"df\"), AIC = AIC(m_ml$lme), AICc = aic_corrige(m_ml$lme, nrow(cadre)), BIC = BIC(m_ml$lme))"
    else "criteres <- c(logLik = as.numeric(ll), parametres = attr(ll, \"df\"), AIC = AIC(m_ml), AICc = aic_corrige(m_ml, nrow(cadre)), BIC = BIC(m_ml))",
    "",
    commentaire(tx(sprintf("Chaque observation : la réponse observée, ajustée%s, et son résidu standardisé%s.", if (mixte && (!mgcv || gamm)) " (avec les effets aléatoires ; ajuste_fixe : les effets fixes seuls)" else "",
                           if (!mixte && !mgcv) " (et la distance de Cook)" else ""),
                   sprintf("Each observation: the response observed, fitted%s, and its standardized residual%s.", if (mixte && (!mgcv || gamm)) " (with the random effects; fitted_fixed: the fixed effects only)" else "",
                           if (!mixte && !mgcv) " (and Cook's distance)" else ""))),
    if (gamm) "observations <- data.frame(observation = noms_obs, observe = cadre[[1]], ajuste = fitted(m$lme),"
    else "observations <- data.frame(observation = noms_obs, observe = cadre[[1]], ajuste = fitted(m),",
    if (gamm) sprintf("                           residu = residus_normalises_car1(residuals(m$lme, type = \"pearson\"), %s, %s, phi), ajuste_fixe = fitted(m$gam), row.names = NULL)",
                      temps_car1, groupes_car1_code)
    else if (mgcv) "                           residu = residuals(m, type = \"deviance\") / sqrt(m$sig2 * pmax(1 - m$hat, 1e-8)), row.names = NULL)"
    else if (mixte && car1) sprintf("                           residu = residus_normalises_car1(residuals(m, type = \"pearson\"), %s, %s, phi), ajuste_fixe = fitted(m, level = 0), row.names = NULL)",
                                    temps_car1, groupes_car1_code)
    else if (mixte) "                           residu = residuals(m, type = \"normalized\"), ajuste_fixe = fitted(m, level = 0), row.names = NULL)"
    else "                           residu = rstandard(m), cook = cooks.distance(m), row.names = NULL)")
  if (quantiles) {
    # (la loi du modèle à ses valeurs ajustées, comme regression_diagnostics.R : residus_quantiles ; continue pour la loi gamma)
    arguments <- c(poisson = "function(q, ...) ppois(q, fitted(m), ...)",
                   binomiale_negative = "function(q, ...) pnbinom(q, size = m$family$getTheta(TRUE), mu = fitted(m), ...)",
                   binomiale = "function(q, ...) pbinom(q, 1, fitted(m), ...)",
                   gamma = "function(q, ...) pgamma(q, shape = 1 / m$sig2, rate = 1 / (m$sig2 * fitted(m)), ...), discrete = FALSE")[[pl$famille]]
    lois <- c(poisson = "Poisson", binomiale_negative = tx("binomiale négative, de son θ estimé", "negative binomial with its estimated θ"),
              binomiale = tx("binomiale, une épreuve : 0 ou 1", "binomial, one trial: 0 or 1"), gamma = tx("gamma de forme 1 / l'échelle estimée", "gamma of shape 1 / the estimated scale"))
    statistiques <- c(statistiques,
      commentaire(tx(sprintf("Et ses résidus quantiles randomisés (residus_quantiles ci-dessus), de la loi du modèle (%s) à sa valeur ajustée : normaux quand le modèle est juste.", lois[[pl$famille]]),
                     sprintf("And its randomized quantile residuals (quantile_residuals above), from the model's distribution (%s) at its fitted value: normal when the model is right.", lois[[pl$famille]]))),
      sprintf("observations$residu_quantile <- residus_quantiles(cadre[[1]], %s)", arguments))
  }

  # --- la sélection par l'AIC (regression.R : selection_aic), quand elle a été faite dans Nuée
  sel <- pl$selection
  code_liste <- function(x, nom) {
    v <- sprintf("%s = %s", vapply(names(x), code_texte, ""), vapply(x, code_textes, ""))
    if (length(v) <= 1) sprintf("%s <- list(%s)", nom, paste(v, collapse = "")) else code_valeurs(v, paste(nom, "<- "), ouvrir = "list(")
  }
  selection <- if (!is.null(sel)) {
    avec <- c(if (mixte) tx("ses effets aléatoires", "its random effects"), if (car1) tx("son autocorrélation", "its autocorrelation"),
              if (!is.null(pl$effort)) tx("son effort", "its effort"), if (courbes) tx("ses courbes et leur dimension", "its curves and their dimension"))
    c(commentaire(tx(sprintf("La sélection par l'AIC faite dans Nuée (selection_aic ci-dessus) : %s ; chaque modèle est ajusté en ML sur les lignes du modèle%s, un terme croisé n'entrant qu'avec ses effets principaux (requis_aic).",
                             if (sel$toutes) "chaque combinaison des groupes de termes du modèle (le bloc de chaque terme, sinon sa variable)"
                             else sprintf("une élimination descendante depuis le modèle complet, ses groupes de termes (le bloc de chaque terme, sinon sa variable) donnant plus de %d combinaisons", as.integer(sel$limite)),
                             if (length(avec)) sprintf(", avec %s", liste(avec)) else ""),
                     sprintf("The AIC selection done in Nuée (aic_selection above): %s; each model is fitted by ML on the model's rows%s, a cross term entering only with its main effects (aic_required).",
                             if (sel$toutes) "every combination of the model's groups of terms (each term's block, otherwise its variable)"
                             else sprintf("a backward elimination from the full model, its groups of terms (each term's block, otherwise its variable) giving more than %d combinations", as.integer(sel$limite)),
                             if (length(avec)) sprintf(", with %s", liste(avec)) else ""))),
      code_liste(sel$groupes, "groupes_aic"),
      code_liste(sel$requis, "requis_aic"),
      # (les termes des groupes qui portent une pente aléatoire : dans chaque modèle, hors des groupes à choisir)
      if (length(sel$gardes)) sprintf("termes_gardes <- %s  # %s", code_textes(sel$gardes),
                                      tx("une pente aléatoire suit la pente fixe de son terme : dans chaque modèle", "a random slope goes with its term's fixed slope: in every model")),
      sprintf("ajuster_ml <- function(termes_aic) %s", sub("termes_aic", if (length(sel$gardes)) "c(termes_gardes, termes_aic)" else "termes_aic",
              if (gamm) "ajuster_modele(cadre, intersect(utiles, termes_aic), methode = \"ML\")$lme"
              else if (mgcv || mixte) "ajuster_modele(cadre, intersect(utiles, termes_aic), methode = \"ML\")"
              else "ajuster_modele(cadre, intersect(utiles, termes_aic))", fixed = TRUE)),
      sprintf("selection <- selection_aic(groupes_aic, requis_aic, ajuster_ml, nrow(cadre)%s)", if (sel$toutes) "" else ", toutes = FALSE"))
  }

  # --- le jeu de test
  predire_test <- if (gamm) "as.vector(predict(m$gam, cadre_test, type = \"response\"))"
                  else if (mgcv) (if (mixte) "predire_fixes(m, cadre_test, cadre, aleatoires)" else "as.vector(predict(m, cadre_test, type = \"response\"))")
                  else if (mixte) "as.vector(predict(m, cadre_test, level = 0))" else "as.vector(predict(m, cadre_test))"
  code_test <- if (test) c(
    commentaire(tx(sprintf("Le jeu de test : ses observations prédites par le modèle%s ; RMSEP, biais (moyenne des erreurs), SEP (leur écart-type) et Q2 externe (par rapport à la moyenne des observations du modèle).",
                           if (mixte) ", par ses seuls effets fixes (il ne connaît pas leurs groupes)" else ""),
                   sprintf("The test set: its observations predicted by the model%s; RMSEP, bias (mean of the errors), SEP (their standard deviation) and external Q2 (against the mean of the model's observations).",
                           if (mixte) ", by its fixed effects only (it doesn't know their groups)" else ""))),
    sprintf("y_predit <- %s", sans_inconnus(predire_test, "cadre", "cadre_test")),
    "erreur <- y_predit - cadre_test[[1]]",
    if (!pentes_sans_effet) c(
      sprintf("test <- data.frame(y = %s, n = length(erreur), RMSEP = sqrt(mean(erreur^2)), biais = mean(erreur),", code_texte(pl$y$nom)),
      "                   SEP = if (length(erreur) > 1) sd(erreur) else NA_real_, Q2ext = 1 - sum(erreur^2) / sum((cadre_test[[1]] - mean(cadre[[1]]))^2))")
    else c(
      sprintf("ok <- !is.na(erreur)  # %s", tx("(sans prédiction : une catégorie que le modèle n'a pas vue)", "(no prediction: a category the model hasn't seen)")),
      sprintf("test <- data.frame(y = %s, n = sum(ok), RMSEP = sqrt(mean(erreur[ok]^2)), biais = mean(erreur[ok]),", code_texte(pl$y$nom)),
      "                   SEP = if (sum(ok) > 1) sd(erreur[ok]) else NA_real_, Q2ext = 1 - sum(erreur[ok]^2) / sum((cadre_test[[1]][ok] - mean(cadre[[1]]))^2))"),
    "predictions_test <- data.frame(observation = noms_test, observe = cadre_test[[1]], predit = y_predit, row.names = NULL)")

  # --- la sortie et les graphiques
  ligne <- function(code, fr, en) {
    code <- traduire_code(code, langue)
    paste0(formatC(code, width = -max(27, nchar(code) + 2)), "# ", tx(fr, en))
  }
  sortie <- c(
    ligne("print(coefficients)", "chaque terme : estimation, erreur-type, intervalle à 95 %, p", "each term: estimate, standard error, 95% interval, p"),
    ligne("print(criteres)", "vraisemblance et AIC (de l'ajustement en ML)", "likelihood and AIC (of the ML fit)"),
    ligne("print(r2)", if (mgcv && !gamm) "R² ajusté, part de la déviance expliquée" else if (mixte) "R² marginal et conditionnel" else "R² et R² ajusté",
          if (mgcv && !gamm) "adjusted R², share of the deviance explained" else if (mixte) "marginal and conditional R²" else "R² and adjusted R²"),
    if (mixte && !avec_pentes) ligne("print(variances)", "la variance de chaque effet aléatoire", "the variance of each random effect"),
    if (avec_pentes) ligne("print(variances)", if (mgcv && !gamm) "chaque ordonnée à l'origine et pente aléatoire" else "chaque ordonnée à l'origine et pente aléatoire, et leur corrélation",
                           if (mgcv && !gamm) "each random intercept and slope" else "each random intercept and slope, and their correlation"),
    if (car1) ligne("print(phi)", "l'autocorrélation CAR1", "the CAR1 autocorrelation"),
    if (loi) ligne("print(parametre_loi)", "le paramètre de la loi", "the parameter of the distribution"),
    if (poisson) ligne("print(dispersion)", "au-delà de 1,5 : préférer la binomiale négative", "above 1.5: prefer the negative binomial"),
    if (courbes) ligne("print(courbes)", "edf, test et concurvité de chaque courbe", "edf, test and concurvity of each curve"),
    if (courbes) ligne("print(plages_seuils)", "les plages nettes de la pente et de l'effet des courbes", "the clear ranges of the curves' slope and effect"),
    ligne("print(c(Q2 = vc$Q2, RMSEcv = vc$RMSEcv))", "validation croisée", "cross-validation"),
    if (!is.null(sel)) ligne("print(head(selection$modeles, 20))", "sélection par l'AIC : les 20 meilleurs modèles", "AIC selection: the 20 best models"),
    if (!is.null(sel)) ligne("print(selection$importance)", "l'importance de chaque groupe de termes", "the importance of each group of terms"),
    if (test) ligne("print(test)", "jeu de test : RMSEP, biais, SEP, Q2 externe", "test set: RMSEP, bias, SEP, external Q2"),
    if (quantiles) ligne("print(head(observations))", "observée, ajustée, résidu standardisé et résidu quantile", "observed, fitted, standardized residual and quantile residual")
    else ligne("print(head(observations))", "observée, ajustée, résidu standardisé", "observed, fitted, standardized residual"),
    ligne(if (gamm) "print(summary(m$gam))" else "print(summary(m))", "le résumé de R", "R's own summary"))
  ylab <- if (identical(pl$lien, "identity")) sprintf(tx("Effet sur %s", "Effect on %s"), pl$y$nom) else sprintf(tx("Effet (%s)", "Effect (%s)"), pl$lien)
  graphiques <- list(
    list(nom = "observe-ajuste", largeur = 5, hauteur = 5, code = c(
      sprintf("plot(observations$ajuste, observations$observe, pch = 19, xlab = %s, ylab = %s, main = %s)", code_texte(tx("Ajusté", "Fitted")),
              code_texte(tx("Observé", "Observed")), code_texte(pl$y$nom)),
      "abline(0, 1, col = \"grey\")")),
    list(nom = "residus", largeur = 8, hauteur = 4, code = c(
      "op <- par(mfrow = c(1, 2))",
      sprintf("plot(observations$ajuste, observations$residu, pch = 19, xlab = %s, ylab = %s, main = %s)", code_texte(tx("Ajusté", "Fitted")),
              code_texte(tx("Résidu standardisé", "Standardized residual")), code_texte(tx("Résidus", "Residuals"))),
      "abline(h = c(-2, 0, 2), lty = c(2, 1, 2), col = \"grey\")",
      sprintf("qqnorm(observations$residu, pch = 19, main = %s)", code_texte(tx("Droite de Henry", "Normal Q-Q plot"))),
      "qqline(observations$residu, col = \"grey\")",
      "par(op)")),
    if (quantiles) list(nom = "residus-quantiles", largeur = 8, hauteur = 4, code = c(
      "op <- par(mfrow = c(1, 2))",
      sprintf("plot(observations$ajuste, observations$residu_quantile, pch = 19, xlab = %s, ylab = %s, main = %s)", code_texte(tx("Ajusté", "Fitted")),
              code_texte(tx("Résidu quantile", "Quantile residual")), code_texte(tx("Résidus quantiles", "Quantile residuals"))),
      "abline(h = c(-2, 0, 2), lty = c(2, 1, 2), col = \"grey\")",
      sprintf("qqnorm(observations$residu_quantile, pch = 19, main = %s)", code_texte(tx("Droite de Henry, résidus quantiles", "Normal Q-Q plot, quantile residuals"))),
      "qqline(observations$residu_quantile, col = \"grey\")",
      "par(op)")),
    if (pl$n_courbes) list(nom = "courbes", largeur = 4 * pl$n_courbes, hauteur = 4, code = c(
      if (pl$n_courbes > 1) sprintf("op <- par(mfrow = c(1, %d))", pl$n_courbes),
      "for (courbe in intersect(lisses, utiles)) {",
      "  j <- match(courbe, termes)",
      "  x <- seq(min(cadre[[courbe]]), max(cadre[[courbe]]), length.out = 80)",
      "  nd <- cadre[rep(1, length(x)), , drop = FALSE]",
      "  nd[[courbe]] <- x",
      sprintf("  pr <- predict(%s, nd, type = \"terms\", terms = sprintf(\"s(%%s)\", courbe), se.fit = TRUE)", if (gamm) "m$gam" else "m"),
      "  effet <- pr$fit[, 1]",
      "  se <- pr$se.fit[, 1]",
      sprintf("  abscisse <- x / poids_x[j] + centre_x[j]  # %s", tx("dans les unités de la variable", "in the variable's units")),
      sprintf("  plot(abscisse, effet, type = \"l\", ylim = range(effet - 1.96 * se, effet + 1.96 * se), xlab = %s, ylab = %s)",
              if (meme_noms) "courbe" else "noms_termes[j]", code_texte(ylab)),
      "  lines(abscisse, effet - 1.96 * se, lty = 2)",
      "  lines(abscisse, effet + 1.96 * se, lty = 2)",
      "  rug(cadre[[courbe]] / poids_x[j] + centre_x[j])",
      "}",
      if (pl$n_courbes > 1) "par(op)")),
    if (pl$n_courbes) list(nom = "seuils", largeur = 4 * pl$n_courbes, hauteur = 6, code = c(
      sprintf("op <- par(mfcol = c(2, %d), mar = c(4, 4, 2, 1))", pl$n_courbes),
      "for (n in names(seuils)) {",
      "  s <- seuils[[n]]",
      sprintf("  plot(s$x, s$effet, type = \"l\", ylim = range(s$bas, s$haut), xlab = \"\", ylab = %s, main = n)", code_texte(ylab)),
      "  polygon(c(s$x, rev(s$x)), c(s$bas, rev(s$haut)), col = \"#80808040\", border = NA)",
      "  abline(h = 0, col = \"grey\")",
      sprintf("  plot(s$x_pente, s$pente, type = \"l\", ylim = range(s$pente_bas, s$pente_haut, 0), xlab = n, ylab = %s)", code_texte(tx("Pente", "Slope"))),
      "  polygon(c(s$x_pente, rev(s$x_pente)), c(s$pente_bas, rev(s$pente_haut)), col = \"#80808040\", border = NA)",
      "  abline(h = 0, col = \"grey\")",
      sprintf("  nettes <- s$plages[s$plages$mesure == %s, ]", code_texte(tx("pente", "slope"))),
      sprintf("  if (nrow(nettes)) rect(nettes$x_debut, par(\"usr\")[3], nettes$x_fin, par(\"usr\")[4], col = ifelse(nettes$sens == %s, \"#2a9d8f33\", \"#d1495b33\"), border = NA)",
              code_texte(tx("hausse", "rise"))),
      "}",
      "par(op)")))
  objets <- c("m", "m_ml", "cadre", if (test) "cadre_test", "coefficients", "criteres", "r2", "vc", "observations",
              if (mixte) "variances", if (car1) "phi", if (loi) "parametre_loi", if (poisson) "dispersion", if (courbes) c("courbes", "seuils", "plages_seuils"),
              if (!is.null(sel)) "selection", if (test) c("test", "predictions_test"))
  list(calcul = traduire_code(c(donnees, "", ajustement, "", validation, "", statistiques, if (!is.null(sel)) c("", selection), if (test) c("", code_test)), langue),
       sortie = sortie, graphiques = lapply(Filter(Negate(is.null), graphiques), function(g) { g$code <- traduire_code(g$code, langue); g }),
       objets = nom_script(objets, langue))
}

# Écart entre les résultats du script d'une régression (dans `e`) et ceux de `m` : NULL s'il n'y en
# a pas, sinon les écarts relatifs de ce qui diffère (coefficients, vraisemblance et AIC, R²,
# variances, paramètres, courbes, validation croisée, valeurs ajustées et résidus, résidus
# quantiles, jeu de test).
ecart_regression <- function(pl, m, e, langue) {
  r <- function(x) get0(nom_script(x, langue), envir = e, inherits = FALSE)
  colonnes <- function(x, k) if (!is.null(x) && length(x) >= max(k)) as.matrix(x[k])
  res <- m$resultat
  vc <- r("vc")
  paires <- list(
    coefficients = list(as.matrix(res$coefficients[c("estimation", "erreur_type", "ddl", "t", "p", "bas", "haut")]), colonnes(r("coefficients"), 2:8)),
    "AIC, vraisemblance" = list(c(res$logLik, res$parametres, res$AIC, res$AICc, res$BIC), r("criteres")),
    R2 = list(unlist(res$r2), r("r2")),
    variances = if (!is.null(res$variances)) list(res$variances$variance, r("variances")$variance),
    # (la corrélation de chaque pente aléatoire avec l'ordonnée à l'origine de son niveau, nlme)
    correlations = if (any(!is.na(res$variances$correlation))) list(res$variances$correlation, r("variances")$correlation),
    phi = if (!is.null(res$phi)) list(res$phi, r("phi")),
    "paramètre de la loi" = if (!is.null(res$parametre_loi)) list(res$parametre_loi$valeur, r("parametre_loi")),
    dispersion = if (!is.null(res$dispersion)) list(res$dispersion, r("dispersion")),
    courbes = if (!is.null(res$courbes)) list(as.matrix(res$courbes[-1]), colonnes(r("courbes"), seq_along(res$courbes)[-1])),
    # (les courbes de ce modèle : dans une séance, `e` garde les seuils d'un modèle précédent)
    "seuils des courbes" = if (!is.null(res$courbes)) list(unlist(lapply(res$courbes$terme, function(n) { s <- seuils_courbe(m, n); c(s$x, s$effet, s$pente) })),
                                                      unlist(lapply(r("seuils")[res$courbes$terme], function(s) s[nom_script(c("x", "effet", "pente"), langue)]))),
    "validation croisée" = list(c(m$vc$Q2, m$vc$RMSEcv, m$vc$echecs), if (!is.null(vc)) c(vc$Q2, vc$RMSEcv, vc[[nom_script("echecs", langue)]])),
    "prédictions de la validation croisée" = list(m$vc$prediction, vc$prediction),
    "valeurs ajustées et résidus" = list(cbind(res$ajuste, res$residus), colonnes(r("observations"), 3:4)),
    "résidus quantiles" = if (!is.null(res$residus_quantiles)) list(res$residus_quantiles, r("observations")[[nom_script("residu_quantile", langue)]]),
    "sélection par l'AIC" = if (!is.null(pl$selection)) list(as.matrix(m$selection_aic$modeles[c("k", "logLik", "AICc", "delta", "poids", "poids_cumule")]),
                                                             colonnes(r("selection")[[nom_script("modeles", langue)]], 2:7)),
    "importance des groupes" = if (!is.null(pl$selection)) list(m$selection_aic$importance$importance, unname(r("selection")[[nom_script("importance", langue)]])),
    test = if (!is.null(m$test)) list(m$test$statistiques[c("n", "RMSEP", "biais", "SEP", "Q2ext")], colonnes(r("test"), 2:6)))
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

# ---------------------------------------------------------------- les fonctions du script

# Les fonctions dont les modèles du script ont besoin (`b` : besoins_script()), en R de base,
# commentées dans la langue du script. Pour chaque type de modèle, l'ajustement (acp, pls, opls)
# et la validation croisée (validation_croisee_acp, _pls, _opls : modele_vc_acp(),
# modele_vc_pls(), modele_vc_opls() ci-dessous) sont des fonctions séparées.
code_fonctions <- function(b, plans, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  na <- b$manquantes
  general <- b$opls_general
  seul <- if (!b$seance) plans[[1]]
  morceaux <- list(
    if (isTRUE(b$mgcv)) c(commentaire(tx("Les modèles généralisés et additifs : mgcv (livré avec R), attaché pour que ses lois (nb(), tw()) trouvent ses fonctions.",
                                         "Generalized and additive models: mgcv (it comes with R), attached so that its distributions (nb(), tw()) find its functions.")),
                          "library(mgcv)"),
    if (na || b$acp) modele_projeter(tx),  # (la validation croisée d'une ACP ignore des termes comme des valeurs manquantes)
    if (b$acp) c(modele_composante_acp(tx, na), "", modele_acp(tx, na, b$vides), "", modele_vc_acp(tx, na)),
    if (b$pls || b$opls) modele_composante_pls(tx, na),
    if (b$pls) c(modele_pls(tx, na, b$vides), "", modele_vc_pls(tx, na, b$y_manquantes)),
    if (b$opls) c(if (general) c(modele_composantes_pls(tx, na), "", modele_base_poids_y(tx, na), ""),
                  modele_opls_noyau(tx, na, general), "", modele_opls(tx, na, b$vides, general), "",
                  modele_opls_predire(tx, na, general), "", modele_vc_opls(tx, na, general)),
    if (!isFALSE(b$echelle)) modele_echelle(tx, !isTRUE(b$regression) || isTRUE(b$nipals)),
    if (isTRUE(b$regression)) c(modele_termes_utiles(tx), "", modele_aic_corrige(tx), "", modele_validation_regression(tx), if (isTRUE(b$car1)) c("", modele_residus_car1(tx)),
                                if (isTRUE(b$dates)) c("", modele_temps_dates(tx)), if (isTRUE(b$pentes)) c("", modele_niveaux_inconnus(tx))),
    if (isTRUE(b$selection_aic)) modele_selection_aic(tx),
    if (isTRUE(b$lmm) || isTRUE(b$gamm)) modele_variances_niveaux(tx),
    if (isTRUE(b$pentes_nlme)) modele_variances_pentes(tx),
    if (isTRUE(b$lmm)) modele_r2_mixte(tx),
    if (isTRUE(b$gamm)) modele_r2_gamm(tx),
    if (isTRUE(b$courbes)) c(modele_dimension_courbe(tx), "", modele_seuils_courbe(tx)),
    if (isTRUE(b$aleatoires_mgcv)) modele_predire_fixes(tx),
    if (isTRUE(b$residus_quantiles)) modele_residus_quantiles(tx),
    if (b$plsda) modele_la_plus_grande(tx),
    if (b$pls || b$opls) modele_permutations(tx, seul, na),
    if ("snv" %in% b$spectres) modele_snv(tx),
    if ("msc" %in% b$spectres) modele_msc(tx),
    if ("sg" %in% b$spectres) modele_savitzky_golay(tx),
    if ("detrend" %in% b$spectres) modele_detrend(tx, na),
    if ("ligne_base" %in% b$spectres) modele_ligne_base(tx),
    if (length(b$fonctions_condition)) utils::head(code_fonctions_condition(b$fonctions_condition, tx, dates = !isTRUE(b$dates)), -1))
  traduire_code(unlist(lapply(Filter(length, morceaux), function(x) c(x, ""))), langue)
}

modele_projeter <- function(tx) c(
  commentaire(tx("Projection de chaque ligne de E sur le vecteur v, sur ses seules valeurs présentes (M vaut 1 où la valeur est présente, 0 où elle manque ; E y vaut 0). Le résultat est multiplié par somme(v^2) / somme des v^2 des valeurs présentes, un facteur limité à 3.",
                 "Projection of each row of E on the vector v, over its present values only (M is 1 where the value is present, 0 where it is missing; E is 0 there). The result is multiplied by sum(v^2) / sum of v^2 over the present values, a factor capped at 3.")),
  "projeter <- function(E, M, v) {",
  "  correction <- pmin(sum(v^2) / pmax(drop(M %*% v^2), 1e-300), 3)",
  "  drop(E %*% v) / sum(v^2) * correction",
  "}")

modele_composante_acp <- function(tx, na) c(
  commentaire(c(tx("Une composante d'ACP par NIPALS, à partir des résidus E : les scores t et les loadings p (de norme 1), recalculés l'un par l'autre jusqu'à ce que t ne change plus (écart relatif 1e-10, 2000 tours au plus).",
                   "One PCA component by NIPALS, from the residuals E: the scores t and the loadings p (of norm 1), computed from each other until t no longer changes (relative change 1e-10, 2000 rounds at most)."),
                if (na) tx("Les valeurs manquantes sont ignorées (M).", "Missing values are ignored (M)."))),
  if (na) "composante_acp <- function(E, M) {" else "composante_acp <- function(E) {",
  if (na) c("  Et <- t(E)", "  Mt <- t(M)"),
  "  tt <- E[, which.max(colSums(E^2))]",
  "  for (i in 1:2000) {",
  if (na) "    p <- projeter(Et, Mt, tt)" else "    p <- drop(crossprod(E, tt)) / sum(tt^2)",
  "    p <- p / sqrt(sum(p^2))",
  if (na) "    t_suivant <- projeter(E, M, p)" else "    t_suivant <- drop(E %*% p) / sum(p^2)",
  "    fini <- sqrt(sum((t_suivant - tt)^2)) <= 1e-10 * sqrt(sum(t_suivant^2))",
  "    tt <- t_suivant",
  "    if (fini) break",
  "  }",
  if (na) "  list(t = tt, p = projeter(Et, Mt, tt))" else "  list(t = tt, p = drop(crossprod(E, tt)) / sum(tt^2))",
  "}")

modele_composante_pls <- function(tx, na) c(
  commentaire(c(tx("Une composante PLS par NIPALS, à partir des résidus E de X et F de Y : les poids w des termes X (de norme 1), les scores t, les poids c des termes Y et les loadings p, recalculés jusqu'à ce que les scores u de Y ne changent plus (écart relatif 1e-10, 2000 tours au plus).",
                   "One PLS component by NIPALS, from the residuals E of X and F of Y: the weights w of the X terms (of norm 1), the scores t, the weights c of the Y terms and the loadings p, computed until the Y scores u no longer change (relative change 1e-10, 2000 rounds at most)."),
                if (na) tx("Les valeurs manquantes sont ignorées (ME, MF).", "Missing values are ignored (ME, MF)."))),
  if (na) "composante_pls <- function(E, ME, F, MF) {" else "composante_pls <- function(E, F) {",
  if (na) c("  Et <- t(E)", "  MEt <- t(ME)", "  Ft <- t(F)", "  MFt <- t(MF)"),
  "  u <- F[, which.max(colSums(F^2))]",
  "  for (i in 1:2000) {",
  if (na) "    w <- projeter(Et, MEt, u)" else "    w <- drop(crossprod(E, u)) / sum(u^2)",
  "    w <- w / sqrt(sum(w^2))",
  if (na) "    tt <- projeter(E, ME, w)" else "    tt <- drop(E %*% w) / sum(w^2)",
  if (na) "    cc <- projeter(Ft, MFt, tt)" else "    cc <- drop(crossprod(F, tt)) / sum(tt^2)",
  if (na) "    u_suivant <- projeter(F, MF, cc)" else "    u_suivant <- drop(F %*% cc) / sum(cc^2)",
  "    fini <- sqrt(sum((u_suivant - u)^2)) <= 1e-10 * sqrt(sum(u_suivant^2))",
  "    u <- u_suivant",
  "    if (fini) break",
  "  }",
  if (na) "  list(w = w, t = tt, c = cc, p = projeter(Et, MEt, tt))" else "  list(w = w, t = tt, c = cc, p = drop(crossprod(E, tt)) / sum(tt^2))",
  "}")

# L'ajustement d'une ACP : les composantes une à une, chacune retirée de X.
modele_acp <- function(tx, na, vides) c(
  commentaire(tx("L'ACP à A composantes (X centré et mis à l'échelle) : chaque composante est retirée de X avant la suivante ; moins de A quand X n'a plus de variation (un reste sous 1e-12 de sa somme des carrés). ssx : la somme des carrés de chaque terme, avant la 1re composante puis après chacune.",
                 "The PCA with A components (X centred and scaled): each component is removed from X before the next; fewer than A when X has no variation left (a remainder under 1e-12 of its sum of squares). ssx: the sum of squares of each term, before the 1st component then after each.")),
  "acp <- function(X, A) {",
  if (na) c("  M <- 1 * !is.na(X)", "  E <- replace(X, is.na(X), 0)") else "  E <- X",
  "  m <- list(ssx = colSums(E^2))",
  "  depart <- sum(E^2)",
  "  for (a in 1:A) {",
  tx("    if (sum(E^2) <= 1e-12 * depart) break  # plus de variation à décrire", "    if (sum(E^2) <= 1e-12 * depart) break  # no variation left to describe"),
  if (na) "    k <- composante_acp(E, M)" else "    k <- composante_acp(E)",
  if (na) "    E <- (E - outer(k$t, k$p)) * M" else "    E <- E - outer(k$t, k$p)",
  "    m$T <- cbind(m$T, k$t)",
  "    m$P <- cbind(m$P, k$p)",
  "    m$ssx <- cbind(m$ssx, colSums(E^2))",
  "  }",
  if (vides) tx("  m$T[rowSums(M) == 0, ] <- NA  # sans aucune valeur X : pas de score", "  m$T[rowSums(M) == 0, ] <- NA  # without any X value: no score"),
  tx("  m$E <- E  # les résidus de X", "  m$E <- E  # the residuals of X"),
  "  m",
  "}")

# La validation croisée d'une ACP.
modele_vc_acp <- function(tx, na) c(
  commentaire(tx("La validation croisée de l'ACP : les observations sont réparties à tour de rôle en G groupes (la 1re, la 8e, la 15e... dans le groupe 1 pour G = 7), ou d'après `groupe`, le groupe de chaque observation ; les termes en G groupes à tour de rôle. Chaque groupe d'observations est prédit par un sous-modèle ajusté sans lui : ses termes prétraités (pretraiter) d'après les autres lignes, puis une ACP à A composantes. Pour chaque groupe de termes, les valeurs de ces termes sont prédites par les scores de chaque observation calculés sans eux (comme des valeurs manquantes), composante après composante ; puis ramenées dans les unités des données (un terme constant dans le sous-modèle : sa valeur constante) et dans celles du modèle complet. PRESS : la somme des carrés des erreurs de prédiction, avec 1, 2... A composantes ; SS : celle de la composante précédente (celle de X pour la 1re).",
                 "The cross-validation of the PCA: the observations are assigned in turn to G groups (the 1st, 8th, 15th... in group 1 for G = 7), or after `group`, each observation's group; the terms to G groups in turn. Each group of observations is predicted by a sub-model fitted without it: its terms preprocessed (preprocess) from the other rows, then a PCA with A components. For each group of terms, the values of these terms are predicted by the scores of each observation computed without them (as missing values), component after component; then brought back to the units of the data (a term constant in the sub-model: its constant value) and to those of the full model. PRESS: the sum of squared prediction errors, with 1, 2... A components; SS: that of the previous component (that of X for the 1st).")),
  "validation_croisee_acp <- function(X, A, pretraiter, G = 7, groupe = (seq_len(nrow(X)) - 1) %% G + 1) {",
  "  complet <- pretraiter(X, seq_len(nrow(X)))",
  "  groupe_terme <- (seq_len(ncol(complet$X)) - 1) %% G + 1",
  "  vc <- list(press = matrix(0, A, ncol(complet$X)))",
  "  for (g in unique(groupe)) {",
  "    hors <- groupe == g",
  "    s <- pretraiter(X, !hors)",
  "    mg <- acp(s$X[!hors, , drop = FALSE], A)",
  "    for (h in unique(groupe_terme)) {",
  "      cases <- groupe_terme == h",
  "      M <- 1 * !is.na(s$X[hors, , drop = FALSE])",
  tx("      M[, cases] <- 0  # les termes du groupe h : comme manquants", "      M[, cases] <- 0  # the terms of group h: as missing"),
  "      E <- replace(s$X[hors, , drop = FALSE], M == 0, 0)",
  "      prediction <- matrix(0, sum(hors), sum(cases))",
  "      for (a in 1:A) {",
  tx("        if (!is.null(mg$P) && a <= ncol(mg$P)) {  # (un sous-modèle sans plus de variation n'a pas d'autre composante)",
     "        if (!is.null(mg$P) && a <= ncol(mg$P)) {  # (a sub-model with no variation left has no further component)"),
  "          tt <- projeter(E, M, mg$P[, a])",
  "          E <- (E - outer(tt, mg$P[, a])) * M",
  "          prediction <- prediction + outer(tt, mg$P[cases, a])",
  "        }",
  "        x <- sweep(sweep(prediction, 2, ifelse(s$poids_x[cases] > 0, s$poids_x[cases], Inf), \"/\"), 2, s$centre_x[cases], \"+\")",
  "        x <- mettre_a_echelle(x, complet$centre_x[cases], complet$poids_x[cases])",
  sprintf("        vc$press[a, cases] <- vc$press[a, cases] + colSums((complet$X[hors, cases, drop = FALSE] - x)^2%s)", if (na) ", na.rm = TRUE" else ""),
  "      }",
  "    }",
  "  }",
  sprintf("  vc$ss <- rbind(colSums(complet$X^2%s), vc$press[-A, , drop = FALSE])", if (na) ", na.rm = TRUE" else ""),
  "  vc",
  "}")

# L'ajustement d'une PLS.
modele_pls <- function(tx, na, vides) c(
  commentaire(tx("La PLS à A composantes (X et Y centrés et mis à l'échelle) : chaque composante est retirée de X et de Y avant la suivante ; moins de A quand X ou Y n'a plus de variation (un reste sous 1e-12 de sa somme des carrés). ssx, ssy : la somme des carrés de chaque terme, avant la 1re composante puis après chacune.",
                 "The PLS with A components (X and Y centred and scaled): each component is removed from X and Y before the next; fewer than A when X or Y has no variation left (a remainder under 1e-12 of its sum of squares). ssx, ssy: the sum of squares of each term, before the 1st component then after each.")),
  "pls <- function(X, Y, A) {",
  if (na) c("  MX <- 1 * !is.na(X)", "  MY <- 1 * !is.na(Y)", "  E <- replace(X, is.na(X), 0)", "  F <- replace(Y, is.na(Y), 0)") else c("  E <- X", "  F <- Y"),
  if (vides) tx("  avec_x <- rowSums(MX) > 0  # les observations qui ont une valeur X au moins", "  avec_x <- rowSums(MX) > 0  # the observations that have one X value at least"),
  "  m <- list(ssx = colSums(E^2), ssy = colSums(F^2))",
  "  depart <- c(sum(E^2), sum(F^2))",
  "  for (a in 1:A) {",
  tx("    if (sum(E^2) <= 1e-12 * depart[1] || sum(F^2) <= 1e-12 * depart[2]) break  # plus de variation à décrire",
     "    if (sum(E^2) <= 1e-12 * depart[1] || sum(F^2) <= 1e-12 * depart[2]) break  # no variation left to describe"),
  if (na) "    k <- composante_pls(E, MX, F, MY)" else "    k <- composante_pls(E, F)",
  if (na) c("    E <- (E - outer(k$t, k$p)) * MX", "    F <- (F - outer(k$t, k$c)) * MY") else c("    E <- E - outer(k$t, k$p)", "    F <- F - outer(k$t, k$c)"),
  "    m$T <- cbind(m$T, k$t)",
  "    m$W <- cbind(m$W, k$w)",
  "    m$P <- cbind(m$P, k$p)",
  "    m$C <- cbind(m$C, k$c)",
  "    m$ssx <- cbind(m$ssx, colSums(E^2))",
  if (vides) "    m$ssy <- cbind(m$ssy, colSums(F[avec_x, , drop = FALSE]^2))" else "    m$ssy <- cbind(m$ssy, colSums(F^2))",
  "  }",
  if (vides) "  m$T[!avec_x, ] <- NA",
  tx("  m$E <- E  # les résidus de X", "  m$E <- E  # the residuals of X"),
  "  m",
  "}")

# La validation croisée d'une PLS.
modele_vc_pls <- function(tx, na, y_na) c(
  commentaire(tx("La validation croisée de la PLS : les observations sont réparties à tour de rôle en G groupes (la 1re, la 8e, la 15e... dans le groupe 1 pour G = 7), ou d'après `groupe`, le groupe de chaque observation. Chaque groupe est prédit par un sous-modèle ajusté sans lui : ses termes prétraités (pretraiter) d'après les autres lignes, puis une PLS à A composantes ; les lignes du groupe, prétraitées de même, sont projetées sur ses composantes une à une. Le Y prédit avec 1, 2... A composantes est ramené dans les unités des données (un terme constant dans le sous-modèle : sa valeur constante), puis dans celles du modèle complet. PRESS : la somme des carrés des erreurs de prédiction ; SS : celle de la composante précédente (celle de Y pour la 1re) ; y_vc : Y prédit en validation croisée avec A composantes.",
                 "The cross-validation of the PLS: the observations are assigned in turn to G groups (the 1st, 8th, 15th... in group 1 for G = 7), or after `group`, each observation's group. Each group is predicted by a sub-model fitted without it: its terms preprocessed (preprocess) from the other rows, then a PLS with A components; the rows of the group, preprocessed likewise, are projected on its components one by one. The Y predicted with 1, 2... A components is brought back to the units of the data (a term constant in the sub-model: its constant value), then to those of the full model. PRESS: the sum of squared prediction errors; SS: that of the previous component (that of Y for the 1st); y_vc: Y predicted in cross-validation with A components.")),
  "validation_croisee_pls <- function(X, Y, A, pretraiter, G = 7, groupe = (seq_len(nrow(X)) - 1) %% G + 1) {",
  "  complet <- pretraiter(X, Y, seq_len(nrow(X)))",
  "  vc <- list(press = matrix(0, A, ncol(complet$Y)), y_vc = 0 * complet$Y)",
  "  for (g in unique(groupe)) {",
  "    hors <- groupe == g",
  "    s <- pretraiter(X, Y, !hors)",
  "    mg <- pls(s$X[!hors, , drop = FALSE], s$Y[!hors, , drop = FALSE], A)",
  if (na) c("    M <- 1 * !is.na(s$X[hors, , drop = FALSE])", "    E <- replace(s$X[hors, , drop = FALSE], M == 0, 0)") else "    E <- s$X[hors, , drop = FALSE]",
  "    prediction <- matrix(0, sum(hors), ncol(complet$Y))",
  "    for (a in 1:A) {",
  tx("      if (!is.null(mg$W) && a <= ncol(mg$W)) {  # (un sous-modèle sans plus de variation n'a pas d'autre composante)",
     "      if (!is.null(mg$W) && a <= ncol(mg$W)) {  # (a sub-model with no variation left has no further component)"),
  if (na) "        tt <- projeter(E, M, mg$W[, a])" else "        tt <- drop(E %*% mg$W[, a]) / sum(mg$W[, a]^2)",
  if (na) "        E <- (E - outer(tt, mg$P[, a])) * M" else "        E <- E - outer(tt, mg$P[, a])",
  "        prediction <- prediction + outer(tt, mg$C[, a])",
  "      }",
  "      y <- sweep(sweep(prediction, 2, ifelse(s$poids_y > 0, s$poids_y, Inf), \"/\"), 2, s$centre_y, \"+\")",
  "      y <- mettre_a_echelle(y, complet$centre_y, complet$poids_y)",
  sprintf("      vc$press[a, ] <- vc$press[a, ] + colSums((complet$Y[hors, , drop = FALSE] - y)^2%s)", if (y_na) ", na.rm = TRUE" else ""),
  "    }",
  "    vc$y_vc[hors, ] <- y",
  "  }",
  sprintf("  vc$ss <- rbind(colSums(complet$Y^2%s), vc$press[-A, , drop = FALSE])", if (y_na) ", na.rm = TRUE" else ""),
  "  vc",
  "}")

# OPLS à plusieurs composantes prédictives : les Ap premières composantes PLS.
modele_composantes_pls <- function(tx, na) c(
  commentaire(tx("Les Ap premières composantes PLS des résidus E (X) et F (Y), chacune retirée avant la suivante ; moins quand X ou Y n'a plus de variation (un reste sous 1e-12 de sa somme des carrés de départ, depart).",
                 "The first Ap PLS components of the residuals E (X) and F (Y), each removed before the next; fewer when X or Y has no variation left (a remainder under 1e-12 of its starting sum of squares, start).")),
  if (na) "composantes_pls <- function(E, ME, F, MF, Ap, depart) {" else "composantes_pls <- function(E, F, Ap, depart) {",
  "  k <- list()",
  "  for (a in 1:Ap) {",
  tx("    if (sum(E^2) <= 1e-12 * depart[1] || sum(F^2) <= 1e-12 * depart[2]) break  # plus de variation à décrire",
     "    if (sum(E^2) <= 1e-12 * depart[1] || sum(F^2) <= 1e-12 * depart[2]) break  # no variation left to describe"),
  if (na) c("    k[[a]] <- composante_pls(E, ME, F, MF)", "    E <- (E - outer(k[[a]]$t, k[[a]]$p)) * ME", "    F <- (F - outer(k[[a]]$t, k[[a]]$c)) * MF")
  else c("    k[[a]] <- composante_pls(E, F)", "    E <- E - outer(k[[a]]$t, k[[a]]$p)", "    F <- F - outer(k[[a]]$t, k[[a]]$c)"),
  "  }",
  "  k",
  "}")

modele_base_poids_y <- function(tx, na) c(
  commentaire(tx("Plusieurs Y : une base orthonormée de l'espace des poids X'y / y'y de chaque colonne de Y (décomposition en valeurs singulières, arrêtée quand il ne reste presque plus rien ; Trygg et Wold, 2002).",
                 "Several Y: an orthonormal basis of the space of the weights X'y / y'y of each column of Y (singular value decomposition, stopped when almost nothing is left; Trygg and Wold, 2002).")),
  if (na) "base_poids_y <- function(E, ME, F) {" else "base_poids_y <- function(E, F) {",
  tx(sprintf("  W <- apply(F, 2, function(y) if (sum(y^2) > 0) %s else numeric(ncol(E)))  # (un Y sans variation : pas de poids)", if (na) "projeter(t(E), t(ME), y)" else "drop(crossprod(E, y)) / sum(y^2)"),
     sprintf("  W <- apply(F, 2, function(y) if (sum(y^2) > 0) %s else numeric(ncol(E)))  # (a Y without variation: no weight)", if (na) "projeter(t(E), t(ME), y)" else "drop(crossprod(E, y)) / sum(y^2)")),
  "  s <- svd(W)",
  "  reste <- rev(cumsum(rev(s$d^2))) / sum(s$d^2)",
  "  s$u[, seq_len(max(1, sum(reste >= 1e-10, na.rm = TRUE))), drop = FALSE]",
  "}")

modele_opls_noyau <- function(tx, na, general) {
  arguments <- paste0(if (na) "E, ME, F, MF" else "E, F", if (general) ", Ap" else "", ", Ao")
  c(commentaire(tx(sprintf("Le cœur de l'OPLS (Trygg et Wold, 2002) : Ao composantes orthogonales retirées de X une à une, puis %s de X ainsi filtré. À chaque étape, la composante PLS de X donne le loading p ; sa part orthogonale %s est le poids orthogonal wo, si bien que les scores orthogonaux to n'ont pas de lien avec Y. predictives[[k + 1]] : %s après k composantes orthogonales. Le calcul s'arrête plus tôt quand X ou Y n'a plus de variation (un reste sous 1e-12 de sa somme des carrés de départ), ou X plus de variation orthogonale à Y.",
                           if (general) "les Ap composantes prédictives, les composantes PLS" else "la composante prédictive, la composante PLS",
                           if (general) "à l'espace des poids de Y (base_poids_y)" else "au poids w", if (general) "les prédictives" else "la prédictive"),
                   sprintf("The core of OPLS (Trygg and Wold, 2002): Ao orthogonal components removed from X one by one, then %s of X thus filtered. At each step, the PLS component of X gives the loading p; its part orthogonal %s is the orthogonal weight wo, so that the orthogonal scores to are unrelated to Y. predictive[[k + 1]]: %s after k orthogonal components. The computation stops earlier when X or Y has no variation left (a remainder under 1e-12 of its starting sum of squares), or X no variation orthogonal to Y.",
                           if (general) "the Ap predictive components, the PLS components" else "the predictive component, the PLS component",
                           if (general) "to the space of the Y weights (y_weight_basis)" else "to the weight w", if (general) "the predictive ones" else "the predictive one"))),
    sprintf("opls_noyau <- function(%s) {", arguments),
    "  o <- list(predictives = list())",
    "  depart <- c(sum(E^2), sum(F^2))",
    if (general) if (na) "  base <- base_poids_y(E, ME, F)" else "  base <- base_poids_y(E, F)",
    "  for (k in 0:Ao) {",
    if (general) c(sprintf("    kp <- composantes_pls(%s, Ap, depart)", if (na) "E, ME, F, MF" else "E, F"),
                   tx("    if (k > 0 && length(kp) < Ap) break  # plus de variation à décrire",
                      "    if (k > 0 && length(kp) < Ap) break  # no variation left to describe"))
    else c(tx("    if (sum(E^2) <= 1e-12 * depart[1] || sum(F^2) <= 1e-12 * depart[2]) break  # plus de variation à décrire",
              "    if (sum(E^2) <= 1e-12 * depart[1] || sum(F^2) <= 1e-12 * depart[2]) break  # no variation left to describe"),
           sprintf("    kp <- composante_pls(%s)", if (na) "E, ME, F, MF" else "E, F")),
    "    o$predictives[[k + 1]] <- kp",
    if (general) "    if (k == Ao || length(kp) < Ap) break" else "    if (k == Ao) break",
    if (general) "    wo <- drop(kp[[1]]$p - base %*% crossprod(base, kp[[1]]$p))" else "    wo <- kp$p - sum(kp$w * kp$p) * kp$w",
    tx(sprintf("    if (sum(wo^2) <= 1e-12 * sum(%s$p^2)) break  # plus de variation de X orthogonale à Y", if (general) "kp[[1]]" else "kp"),
       sprintf("    if (sum(wo^2) <= 1e-12 * sum(%s$p^2)) break  # no variation of X orthogonal to Y left", if (general) "kp[[1]]" else "kp")),
    "    wo <- wo / sqrt(sum(wo^2))",
    if (na) c("    to <- projeter(E, ME, wo)", "    po <- projeter(t(E), t(ME), to)", "    E <- (E - outer(to, po)) * ME")
    else c("    to <- drop(E %*% wo) / sum(wo^2)", "    po <- drop(crossprod(E, to)) / sum(to^2)", "    E <- E - outer(to, po)"),
    "    o$To <- cbind(o$To, to)",
    "    o$Po <- cbind(o$Po, po)",
    "    o$Wo <- cbind(o$Wo, wo)",
    "  }",
    "  o$E <- E",
    "  o",
    "}")
}

# L'ajustement d'une OPLS.
modele_opls <- function(tx, na, vides, general) {
  mx <- if (na) " * MX" else ""
  my <- if (na) " * MY" else ""
  c(commentaire(tx(sprintf("L'OPLS : %s et Ao composantes orthogonales, rangées comme celles d'une PLS (les prédictives d'abord ; les orthogonales n'ont pas de poids c de Y). ssx, ssy : la somme des carrés de chaque terme, avant la 1re composante puis après chacune, dans cet ordre.",
                           if (general) "Ap composantes prédictives" else "une composante prédictive"),
                   sprintf("The OPLS: %s and Ao orthogonal components, arranged as those of a PLS (the predictive ones first; the orthogonal ones have no Y weight c). ssx, ssy: the sum of squares of each term, before the 1st component then after each, in that order.",
                           if (general) "Ap predictive components" else "one predictive component"))),
    sprintf("opls <- function(X, Y, %sAo) {", if (general) "Ap, " else ""),
    if (na) c("  MX <- 1 * !is.na(X)", "  MY <- 1 * !is.na(Y)", "  X <- replace(X, is.na(X), 0)", "  Y <- replace(Y, is.na(Y), 0)"),
    if (general) "  A <- Ap + Ao" else "  A <- 1 + Ao",
    sprintf("  o <- opls_noyau(%s, %sAo)", if (na) "X, MX, Y, MY" else "X, Y", if (general) "Ap, " else ""),
    "  kp <- o$predictives[[Ao + 1]]",
    if (general) c("  colonne <- function(cle) sapply(kp, function(k) k[[cle]])",
                   "  m <- list(T = cbind(colonne(\"t\"), o$To), P = cbind(colonne(\"p\"), o$Po), W = cbind(colonne(\"w\"), o$Wo),",
                   "            C = cbind(colonne(\"c\"), matrix(0, ncol(Y), Ao)), E = o$E)",
                   sprintf(tx("  for (a in 1:Ap) m$E <- (m$E - outer(m$T[, a], m$P[, a]))%s  # les résidus : X filtré, moins les prédictives",
                              "  for (a in 1:Ap) m$E <- (m$E - outer(m$T[, a], m$P[, a]))%s  # the residuals: filtered X, minus the predictive ones"), mx))
    else c("  m <- list(T = cbind(kp$t, o$To), P = cbind(kp$p, o$Po), W = cbind(kp$w, o$Wo),",
           sprintf("            C = cbind(kp$c, matrix(0, ncol(Y), Ao)), E = (o$E - outer(kp$t, kp$p))%s)", mx)),
    if (vides) tx("  avec_x <- rowSums(MX) > 0  # les observations qui ont une valeur X au moins", "  avec_x <- rowSums(MX) > 0  # the observations that have one X value at least"),
    "  E <- X",
    "  F <- Y",
    "  m$ssx <- colSums(E^2)",
    "  m$ssy <- colSums(F^2)",
    "  for (a in 1:A) {",
    sprintf("    E <- if (a == A) m$E else (E - outer(m$T[, a], m$P[, a]))%s", mx),
    sprintf("    if (a <= %s) F <- (F - outer(m$T[, a], m$C[, a]))%s", if (general) "Ap" else "1", my),
    "    m$ssx <- cbind(m$ssx, colSums(E^2))",
    if (vides) "    m$ssy <- cbind(m$ssy, colSums(F[avec_x, , drop = FALSE]^2))" else "    m$ssy <- cbind(m$ssy, colSums(F^2))",
    "  }",
    if (vides) "  m$T[!avec_x, ] <- NA",
    "  m",
    "}")
}

modele_opls_predire <- function(tx, na, general) c(
  commentaire(tx(sprintf("Le Y prédit (centré et mis à l'échelle) pour les lignes E par un cœur d'OPLS, avec ses k premières composantes orthogonales%s.", if (general) " et ses a premières prédictives" else ""),
                 sprintf("The Y predicted (centred and scaled) for the rows E by an OPLS core, with its first k orthogonal components%s.", if (general) " and its first a predictive ones" else ""))),
  sprintf("opls_predire <- function(o, E, %sk%s) {", if (na) "M, " else "", if (general) ", a" else ""),
  "  for (j in seq_len(k)) {",
  if (na) c("    to <- projeter(E, M, o$Wo[, j])", "    E <- (E - outer(to, o$Po[, j])) * M")
  else c("    to <- drop(E %*% o$Wo[, j]) / sum(o$Wo[, j]^2)", "    E <- E - outer(to, o$Po[, j])"),
  "  }",
  "  kp <- o$predictives[[k + 1]]",
  if (general) c("  Y <- 0",
                 "  for (b in 1:a) {",
                 if (na) "    tt <- projeter(E, M, kp[[b]]$w)" else "    tt <- drop(E %*% kp[[b]]$w) / sum(kp[[b]]$w^2)",
                 "    Y <- Y + outer(tt, kp[[b]]$c)",
                 if (na) "    E <- (E - outer(tt, kp[[b]]$p)) * M" else "    E <- E - outer(tt, kp[[b]]$p)",
                 "  }",
                 "  Y")
  else if (na) "  outer(projeter(E, M, kp$w), kp$c)" else "  outer(drop(E %*% kp$w) / sum(kp$w^2), kp$c)",
  "}")

# La validation croisée d'une OPLS.
modele_vc_opls <- function(tx, na, general) c(
  commentaire(tx(sprintf("La validation croisée de l'OPLS : les observations sont réparties à tour de rôle en G groupes, ou d'après `groupe`, le groupe de chaque observation. Chaque groupe est prédit par un sous-modèle ajusté sans lui : ses termes prétraités (pretraiter) d'après les autres lignes, puis une OPLS aux mêmes composantes (moins si ses données n'en permettent pas autant : il prédit avec celles qu'il a) ; les lignes du groupe, prétraitées de même, sont prédites avec %s puis avec 1, 2... Ao orthogonales. Le Y prédit est ramené dans les unités des données (un terme constant dans le sous-modèle : sa valeur constante), puis dans celles du modèle complet. PRESS de chaque composante ; SS : celui de la précédente (la somme des carrés de Y pour la 1re). y_vc : Y prédit en validation croisée.",
                         if (general) "1, 2... Ap composantes prédictives," else "la composante prédictive seule,"),
                 sprintf("The cross-validation of the OPLS: the observations are assigned in turn to G groups, or after `group`, each observation's group. Each group is predicted by a sub-model fitted without it: its terms preprocessed (preprocess) from the other rows, then an OPLS with the same components (fewer if its data do not allow as many: it predicts with those it has); the rows of the group, preprocessed likewise, are predicted with %s then with 1, 2... Ao orthogonal ones. The predicted Y is brought back to the units of the data (a term constant in the sub-model: its constant value), then to those of the full model. PRESS of each component; SS: that of the previous one (the sum of squares of Y for the 1st). y_vc: Y predicted in cross-validation.",
                         if (general) "1, 2... Ap predictive components," else "the predictive component alone,"))),
  sprintf("validation_croisee_opls <- function(X, Y, %sAo, pretraiter, G = 7, groupe = (seq_len(nrow(X)) - 1) %%%% G + 1) {", if (general) "Ap, " else ""),
  if (general) "  A <- Ap + Ao" else "  A <- 1 + Ao",
  "  complet <- pretraiter(X, Y, seq_len(nrow(X)))",
  "  vc <- list(press = matrix(0, A, ncol(complet$Y)), y_vc = 0 * complet$Y)",
  "  for (g in unique(groupe)) {",
  "    hors <- groupe == g",
  "    s <- pretraiter(X, Y, !hors)",
  if (na) c("    MX <- 1 * !is.na(s$X)", "    MY <- 1 * !is.na(s$Y)", "    s$X <- replace(s$X, MX == 0, 0)", "    s$Y <- replace(s$Y, MY == 0, 0)",
            sprintf("    og <- opls_noyau(s$X[!hors, , drop = FALSE], MX[!hors, , drop = FALSE], s$Y[!hors, , drop = FALSE], MY[!hors, , drop = FALSE], %sAo)", if (general) "Ap, " else ""))
  else sprintf("    og <- opls_noyau(s$X[!hors, , drop = FALSE], s$Y[!hors, , drop = FALSE], %sAo)", if (general) "Ap, " else ""),
  tx("    ao <- length(og$predictives) - 1  # ses composantes orthogonales (moins quand ses données n'ont plus de variation)",
     "    ao <- length(og$predictives) - 1  # its orthogonal components (fewer when its data have no variation left)"),
  "    for (r in 1:A) {",
  if (general) c("      k <- min(max(0, r - Ap), ao)",
                 "      a <- min(r, Ap, length(og$predictives[[k + 1]]))",
                 sprintf("      prediction <- if (a == 0) matrix(0, sum(hors), ncol(complet$Y)) else opls_predire(og, s$X[hors, , drop = FALSE], %sk, a)", if (na) "MX[hors, , drop = FALSE], " else ""))
  else sprintf("      prediction <- if (ao < 0) matrix(0, sum(hors), ncol(complet$Y)) else opls_predire(og, s$X[hors, , drop = FALSE], %smin(r - 1, ao))", if (na) "MX[hors, , drop = FALSE], " else ""),
  "      y <- sweep(sweep(prediction, 2, ifelse(s$poids_y > 0, s$poids_y, Inf), \"/\"), 2, s$centre_y, \"+\")",
  "      y <- mettre_a_echelle(y, complet$centre_y, complet$poids_y)",
  sprintf("      vc$press[r, ] <- vc$press[r, ] + colSums((complet$Y[hors, , drop = FALSE] - y)^2%s)", if (na) ", na.rm = TRUE" else ""),
  "    }",
  "    vc$y_vc[hors, ] <- y",
  "  }",
  sprintf("  vc$ss <- rbind(colSums(complet$Y^2%s), vc$press[-A, , drop = FALSE])", if (na) ", na.rm = TRUE" else ""),
  "  vc",
  "}")

modele_echelle <- function(tx, nommer = TRUE) c(
  if (nommer) commentaire(tx("Chaque colonne de X centrée puis multipliée par son poids : (x - centre) * poids ; et des noms de colonnes numérotés (t1, t2...).",
                             "Each column of X centred then multiplied by its weight: (x - centre) * weight; and numbered column names (t1, t2...)."))
  else commentaire(tx("Chaque colonne de X centrée puis multipliée par son poids : (x - centre) * poids.", "Each column of X centred then multiplied by its weight: (x - centre) * weight.")),
  "mettre_a_echelle <- function(X, centre, poids) sweep(sweep(X, 2, centre), 2, poids, \"*\")",
  if (nommer) c("nommer <- function(M, prefixe) {",
                "  colnames(M) <- paste0(prefixe, seq_len(ncol(M)))",
                "  M",
                "}"))

modele_la_plus_grande <- function(tx) c(
  commentaire(tx("La classe de chaque ligne de Y : celle de sa plus grande valeur.", "The class of each row of Y: that of its largest value.")),
  "la_plus_grande <- function(Y) colnames(Y)[apply(Y, 1, function(v) which.max(v)[1])]")

# Les fonctions des régressions (regression.R) : les termes utiles, l'AIC corrigé, la validation
# croisée ; le R² d'un modèle mixte, la dimension d'une courbe, les prédictions par les effets fixes
# d'un modèle de mgcv.
modele_termes_utiles <- function(tx) c(
  commentaire(tx("Les termes qui peuvent entrer dans un ajustement sur les lignes de `tab` : sans les constants ni ceux qui se déduisent des autres (une catégorie quand toutes y sont, des proportions qui font 1...), dans leur ordre.",
                 "The terms that can enter a fit on the rows of `tab`: without the constant ones nor those that follow from the others (a category when all of them are there, proportions that sum to 1...), in their order.")),
  "termes_utiles <- function(tab, termes) {",
  "  if (!length(termes)) return(character(0))",
  "  q <- qr(cbind(1, as.matrix(tab[termes])), tol = 1e-9)",
  "  garde <- sort(q$pivot[seq_len(q$rank)])",
  "  termes[setdiff(garde, 1) - 1]",
  "}")

# Les lignes d'une catégorie que l'ajustement n'a pas vue, dans une pente sans effet principal :
# celles de regression.R (niveaux_inconnus), écrites pour le script.
modele_niveaux_inconnus <- function(tx) c(
  commentaire(tx("Les lignes de `nouvelles` d'une catégorie que les lignes `appris` n'ont pas, dans l'une des `pentes` (la pente de chaque catégorie, sans l'effet principal de leur variable quantitative) : sa colonne est constante sur `appris`, et l'ajustement l'a laissée de côté. Prédites, elles auraient une pente nulle, celle d'aucune catégorie : elles restent sans prédiction, comme dans Nuée (R refuse de même un niveau qu'il n'a pas vu).",
                 "The rows of `new_rows` of a category the rows `learnt` don't have, in one of the `slopes` (each category's slope, without the main effect of their quantitative variable): its column is constant on `learnt`, and the fit left it out. Predicted, they would have a zero slope, that of no category: they stay without a prediction, as in Nuée (R likewise refuses a level it hasn't seen).")),
  "niveaux_inconnus <- function(appris, nouvelles, pentes) {",
  "  sans <- rep(FALSE, nrow(nouvelles))",
  "  for (k in pentes) {",
  "    v <- appris[[k]]",
  "    tol <- 1e-9 * max(1, abs(v))",
  "    if (any(abs(v - v[1]) > tol)) next",
  "    sans <- sans | (!is.na(nouvelles[[k]]) & abs(nouvelles[[k]] - v[1]) > tol)",
  "  }",
  "  sans",
  "}")

# Les résidus normalisés d'un modèle avec CAR1, dans l'ordre du temps de chaque groupe (ceux de
# regression_diagnostics.R : residus_normalises_car1).
modele_residus_car1 <- function(tx) c(
  commentaire(tx("Les résidus normalisés d'un modèle avec CAR1, dans l'ordre du temps de chaque groupe de l'autocorrélation : à chaque pas, le résidu de Pearson moins sa part attendue du précédent (phi^écart de temps), divisé par la racine de 1 moins le carré de cette part. residuals(m, type = \"normalized\") de nlme ne les donne justes que sur des lignes rangées par groupe, puis par temps.",
                 "The normalized residuals of a model with CAR1, in each autocorrelation group's time order: at each step, the Pearson residual minus its expected share of the previous one (phi^time gap), divided by the square root of 1 minus the square of that share. nlme's residuals(m, type = \"normalized\") gets them right only on rows sorted by group, then by time.")),
  "residus_normalises_car1 <- function(e, tps, g, phi) {",
  "  z <- e",
  "  for (i in split(seq_along(e), factor(g), drop = TRUE)) {",
  "    if (length(i) < 2) next",
  "    i <- i[order(tps[i])]",
  "    r <- phi^diff(tps[i])",
  "    z[i[-1]] <- (e[i[-1]] - r * e[i[-length(i)]]) / sqrt(1 - r^2)",
  "  }",
  "  z",
  "}")

# Le temps fait de dates, en années décimales : le calcul d'annees_decimales (variables.R), écrit
# pour le script, les mêmes opérations dans le même ordre (les mêmes nombres, à la dernière
# décimale). Les valeurs sont celles que Nuée a lues : il ne les vérifie pas.
modele_temps_dates <- function(tx) c(
  commentaire(tx("Le temps, fait de dates, en années décimales comme Nuée les lit : 2019-05-14 (au besoin avec l'heure, 2019-05-14 10:30) ou 14/05/2019 (toujours le jour d'abord ; aussi 14-05-2019 et 14.05.2019) donnent l'année + (le jour de l'année - 1 + la fraction du jour) / le nombre de jours de l'année (366 les années bissextiles). Le 1er janvier est l'année elle-même, et φ est d'une année à l'autre. Une valeur qui n'est pas une date reste manquante.",
                 "The time, made of dates, as decimal years as Nuée reads them: 2019-05-14 (with the time if needed, 2019-05-14 10:30) or 14/05/2019 (always day first; also 14-05-2019 and 14.05.2019) give the year + (the day of the year - 1 + the fraction of the day) / the number of days in the year (366 in leap years). 1 January is the year itself, and φ is from one year to the next. A value that isn't a date stays missing.")),
  "temps_en_nombres <- function(x) {",
  "  x <- trimws(as.character(x))",
  "  x[is.na(x)] <- \"\"",
  tx("  # les morceaux de chaque date, une colonne chacun : année, mois, jour, heure, minutes, secondes",
     "  # the parts of each date, a column each: year, month, day, hour, minutes, seconds"),
  "  morceaux <- function(motif, numeros) {",
  "    m <- regexpr(motif, x, perl = TRUE)",
  "    debut <- attr(m, \"capture.start\")[, numeros, drop = FALSE]",
  "    fin <- debut + attr(m, \"capture.length\")[, numeros, drop = FALSE] - 1L",
  "    p <- matrix(suppressWarnings(as.numeric(substring(rep(x, length(numeros)), debut, fin))), ncol = length(numeros))",
  "    p[m == -1L, ] <- NA",
  "    p",
  "  }",
  "  heure <- \"([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}))?\"",
  tx("  # les dates ISO (année-mois-jour), puis celles écrites le jour d'abord (jma : jour/mois/année)",
     "  # the ISO dates (year-month-day), then those written day first (day/month/year)"),
  "  p <- morceaux(sprintf(\"^([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})(?:[ T]%s)?$\", heure), 1:6)",
  "  jma <- morceaux(sprintf(\"^([0-9]{1,2})([/.-])([0-9]{1,2})\\\\2([0-9]{4})(?: %s)?$\", heure), c(4, 3, 1, 5, 6, 7))",
  "  p[is.na(p[, 1]), ] <- jma[is.na(p[, 1]), ]",
  "  p[, 4:6][is.na(p[, 4:6])] <- 0",
  "  a <- p[, 1]",
  "  bissextile <- (a %% 4 == 0 & a %% 100 != 0) | a %% 400 == 0",
  "  jour_annee <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)[p[, 2]] + p[, 3] + (p[, 2] > 2 & bissextile)",
  "  a + (jour_annee - 1 + (p[, 4] * 3600 + p[, 5] * 60 + p[, 6]) / 86400) / (365 + bissextile)",
  "}")

modele_aic_corrige <- function(tx) c(
  commentaire(tx("AIC corrigé pour les petits échantillons (Hurvich et Tsai, 1989) ; n : le nombre d'observations.",
                 "AIC corrected for small samples (Hurvich & Tsai, 1989); n: the number of observations.")),
  "aic_corrige <- function(m, n) {",
  "  k <- attr(logLik(m), \"df\")",
  "  AIC(m) + if (n - k - 1 > 0) 2 * k * (k + 1) / (n - k - 1) else NA_real_",
  "}")

# La sélection des effets fixes par l'AICc (regression.R : selection_aic), écrite pour le script.
modele_selection_aic <- function(tx) c(
  commentaire(tx("La sélection des effets fixes par l'AICc (Burnham et Anderson, 2002), comme dans Nuée : chaque combinaison des groupes de termes `groupes_aic` (une liste nommée de termes), du modèle nul au modèle complet, un groupe n'entrant qu'avec ceux que `requis_aic` lui demande (les effets principaux d'un terme croisé) ; ou, avec `toutes = FALSE`, une élimination descendante depuis le modèle complet (à chaque pas, le retrait qui baisse le plus l'AICc, jusqu'à ce qu'aucun ne la baisse). `ajuster(termes)` : l'ajustement en ML avec ces termes ; n : le nombre d'observations. Les modèles sont rangés par AICc, avec leur ΔAICc, leur poids d'Akaike et le cumul des poids ; l'importance d'un groupe est la somme des poids des modèles qui le contiennent.",
                 "The selection of the fixed effects by AICc (Burnham & Anderson, 2002), as in Nuée: every combination of the groups of terms `aic_groups` (a named list of terms), from the null model to the full model, a group entering only with those `aic_required` asks of it (the main effects of a cross term); or, with `all_subsets = FALSE`, a backward elimination from the full model (at each step, the removal that lowers AICc the most, until none lowers it). `fit(terms)`: the ML fit with these terms; n: the number of observations. The models are ranked by AICc, with their ΔAICc, their Akaike weight and the cumulative weight; a group's importance is the sum of the weights of the models that contain it.")),
  "selection_aic <- function(groupes_aic, requis_aic, ajuster, n, toutes = TRUE) {",
  "  criteres_aic <- function(choisis) {",
  "    a <- ajuster(unlist(groupes_aic[choisis], use.names = FALSE))",
  "    if (is.null(a)) return(c(k = NA_real_, logLik = NA_real_, AICc = NA_real_))",
  "    ll <- logLik(a)",
  "    c(k = attr(ll, \"df\"), logLik = as.numeric(ll), AICc = aic_corrige(a, n))",
  "  }",
  "  permise <- function(choisis) all(unlist(requis_aic[choisis]) %in% choisis)",
  "  if (toutes) {",
  "    combinaisons <- lapply(0:length(groupes_aic), function(k) combn(names(groupes_aic), k, simplify = FALSE))",
  "    combinaisons <- Filter(permise, unlist(combinaisons, recursive = FALSE))",
  "    valeurs_aic <- t(vapply(combinaisons, criteres_aic, c(k = 0, logLik = 0, AICc = 0)))",
  "    etapes_aic <- rep(NA_integer_, length(combinaisons))",
  "  } else {",
  "    courant <- names(groupes_aic)",
  "    combinaisons <- list(courant)",
  "    valeurs_aic <- t(criteres_aic(courant))",
  "    etapes_aic <- 0L",
  "    aicc_courant <- valeurs_aic[1, \"AICc\"]",
  "    repeat {",
  "      retirables <- setdiff(courant, unlist(requis_aic[courant]))",
  "      if (!length(retirables)) break",
  "      essais <- lapply(retirables, function(g) setdiff(courant, g))",
  "      v <- t(vapply(essais, criteres_aic, c(k = 0, logLik = 0, AICc = 0)))",
  "      combinaisons <- c(combinaisons, essais)",
  "      valeurs_aic <- rbind(valeurs_aic, v)",
  "      etapes_aic <- c(etapes_aic, rep(max(etapes_aic) + 1L, length(essais)))",
  "      j <- which.min(v[, \"AICc\"])",
  "      if (!length(j) || !(is.na(aicc_courant) || v[j, \"AICc\"] < aicc_courant)) break",
  "      courant <- essais[[j]]",
  "      aicc_courant <- v[j, \"AICc\"]",
  "    }",
  "  }",
  "  ordre <- order(valeurs_aic[, \"AICc\"], valeurs_aic[, \"k\"], na.last = TRUE)",
  "  valeurs_aic <- valeurs_aic[ordre, , drop = FALSE]",
  "  combinaisons <- combinaisons[ordre]",
  "  ok <- !is.na(valeurs_aic[, \"AICc\"])",
  "  delta <- valeurs_aic[, \"AICc\"] - min(valeurs_aic[, \"AICc\"], na.rm = TRUE)",
  "  poids <- exp(-delta / 2) / sum(exp(-delta[ok] / 2))",
  "  libelle_aic <- function(g) if (length(g)) paste(g, collapse = \" + \") else \"1\"",
  "  modeles_aic <- data.frame(combinaison = vapply(combinaisons, libelle_aic, \"\"), valeurs_aic, delta = delta,",
  "                            poids = poids, poids_cumule = ifelse(ok, cumsum(ifelse(ok, poids, 0)), NA_real_),",
  "                            etape = etapes_aic[ordre], row.names = NULL)",
  "  contient_aic <- function(g) vapply(combinaisons, function(x) g %in% x, TRUE)",
  "  importance_aic <- vapply(names(groupes_aic), function(g) sum(poids[ok & contient_aic(g)]), 0)",
  "  list(modeles = modeles_aic, importance = importance_aic)",
  "}")

modele_validation_regression <- function(tx) c(
  commentaire(tx("Validation croisée d'une régression : chaque groupe prédit par le modèle ajusté sans lui (`ajuster`, sur les autres lignes de `cadre`) ; `predire(m, nouvelles, appris)` donne ses prédictions. Un ajustement qui échoue laisse son groupe sans prédiction (`echecs`). Q2 et RMSEcv sur la réponse, la première colonne de `cadre`.",
                 "Cross-validation of a regression: each group predicted by the model fitted without it (`fit`, on the other rows of `frame`); `predict_group(m, new_rows, learnt)` gives its predictions. A fit that fails leaves its group without prediction (`failures`). Q2 and RMSEcv on the response, the first column of `frame`.")),
  "validation_croisee <- function(cadre, groupe, ajuster, predire) {",
  "  y <- cadre[[1]]",
  "  prediction <- rep(NA_real_, nrow(cadre))",
  "  echecs <- 0",
  "  for (g in sort(unique(groupe))) {",
  "    ici <- groupe == g",
  "    appris <- droplevels(cadre[!ici, , drop = FALSE])",
  "    m <- ajuster(appris)",
  "    if (is.null(m)) {",
  "      echecs <- echecs + 1",
  "      next",
  "    }",
  "    prediction[ici] <- predire(m, cadre[ici, , drop = FALSE], appris)",
  "  }",
  "  ok <- !is.na(prediction)",
  "  list(prediction = prediction, echecs = echecs,",
  "       Q2 = if (any(ok)) 1 - sum((y[ok] - prediction[ok])^2) / sum((y[ok] - mean(y[ok]))^2) else NA_real_,",
  "       RMSEcv = if (any(ok)) sqrt(mean((y[ok] - prediction[ok])^2)) else NA_real_)",
  "}")

modele_r2_mixte <- function(tx) c(
  commentaire(tx("R² d'un modèle mixte (Nakagawa et Schielzeth, 2013) : marginal, la part de variance des effets fixes ; conditionnel, celle des effets fixes et aléatoires ensemble (variances_niveaux ci-dessus).",
                 "R² of a mixed model (Nakagawa & Schielzeth, 2013): marginal, the share of variance of the fixed effects; conditional, that of the fixed and random effects together (level_variances above).")),
  "r2_mixte <- function(m) {",
  "  fixes <- var(as.vector(model.matrix(formula(m), m$data) %*% nlme::fixef(m)))",
  "  aleatoires <- sum(variances_niveaux(m, names(m$modelStruct$reStruct), m$data))",
  "  totale <- fixes + aleatoires + m$sigma^2",
  "  c(marginal = fixes / totale, conditionnel = (fixes + aleatoires) / totale)",
  "}")

# La variance de chaque niveau d'effets aléatoires d'un modèle de nlme (Johnson, 2014, avec des
# pentes), pour r2_mixte et r2_gamm.
modele_variances_niveaux <- function(tx) c(
  commentaire(tx("La variance de chaque niveau d'effets aléatoires `niveaux_aleatoires` d'un modèle de nlme `l`, sur les lignes `tab` : Σ, σ² fois sa matrice relative, et z = (1, ses pentes aléatoires) ; la moyenne sur les lignes de z'Σz (Johnson, 2014 : avec une pente aléatoire, la variance d'un niveau change avec x) ; sans pente, la variance de son ordonnée à l'origine.",
                 "The variance of each level of random effects `random_levels` of an nlme model `l`, over the rows `tab`: Σ, σ² times its relative matrix, and z = (1, its random slopes); the mean over the rows of z'Σz (Johnson, 2014: with a random slope, a level's variance changes with x); without a slope, the variance of its intercept.")),
  "variances_niveaux <- function(l, niveaux_aleatoires, tab) vapply(niveaux_aleatoires, function(n) {",
  "  S <- l$sigma^2 * as.matrix(l$modelStruct$reStruct[[n]])",
  "  Z <- model.matrix(formula(l$modelStruct$reStruct[[n]]), tab)",
  "  mean(rowSums((Z %*% S) * Z))",
  "}, 0)")

# Le tableau des variances d'un modèle de nlme à pentes aléatoires, comme Nuée le donne.
modele_variances_pentes <- function(tx) c(
  commentaire(tx("Les variances des effets aléatoires d'un modèle de nlme `l` à pentes aléatoires : pour chaque niveau de `niveaux_aleatoires` (nommé par `noms_niveaux`), Σ, σ² fois sa matrice relative ; une ligne par terme, son ordonnée à l'origine puis ses pentes, sa variance et la corrélation de chaque pente avec l'ordonnée à l'origine ; puis la variance résiduelle (`nom_residuelle`).",
                 "The variances of the random effects of an nlme model `l` with random slopes: for each level of `random_levels` (labelled by `level_labels`), Σ, σ² times its relative matrix; one row per term, its intercept then its slopes, its variance and each slope's correlation with the intercept; then the residual variance (`residual_label`).")),
  "variances_pentes <- function(l, niveaux_aleatoires, noms_niveaux, nom_residuelle) {",
  "  lignes <- lapply(seq_along(niveaux_aleatoires), function(j) {",
  "    S <- l$sigma^2 * as.matrix(l$modelStruct$reStruct[[niveaux_aleatoires[j]]])",
  "    data.frame(composante = noms_niveaux[j], terme = rownames(S), variance = diag(S),",
  "               correlation = c(NA, S[-1, 1] / sqrt(S[1, 1] * diag(S)[-1])), row.names = NULL)",
  "  })",
  "  rbind(do.call(rbind, lignes), data.frame(composante = nom_residuelle, terme = \"\", variance = l$sigma^2, correlation = NA))",
  "}")

modele_dimension_courbe <- function(tx) c(
  commentaire(tx("La dimension de base d'une courbe : `k` s'il est donné, sinon 10 au plus ; moins quand la variable a peu de valeurs.",
                 "The basis dimension of a curve: `k` if given, else 10 at most; less when the variable has few values.")),
  "dimension_courbe <- function(x, k = NA) min(if (!is.na(k)) as.integer(k) else 10L, length(unique(x)) - 1L)")

modele_seuils_courbe <- function(tx) c(
  commentaire(tx("Les seuils d'une courbe (Simpson, 2018) : son effet et sa pente sur 200 points du 2e au 98e centile de la variable, avec des intervalles simultanés à `niveau` tirés de `tirages` jeux de coefficients (MASS::mvrnorm) ; `centre` et `poids` ramènent la variable dans ses unités. Les plages où l'intervalle de la pente exclut 0 sont une hausse ou une baisse nette ; celles où l'intervalle de l'effet exclut 0, au-dessus ou au-dessous de la moyenne. Une courbe d'edf sous 1,5 est presque une droite.",
                 "The thresholds of a curve (Simpson, 2018): its effect and slope at 200 points from the 2nd to the 98th percentile of the variable, with simultaneous intervals at `level` from `draws` sets of coefficients (MASS::mvrnorm); `centre` and `weight` bring the variable back to its units. The ranges where the slope's interval excludes 0 are a clear rise or fall; those where the effect's interval excludes 0, above or below the mean. A curve with an edf below 1.5 is nearly a straight line.")),
  "seuils_courbe <- function(g, courbe, cadre, centre, poids, tirages = 2000, niveau = 0.95, graine = 1) {",
  "  grille <- seq(quantile(cadre[[courbe]], 0.02), quantile(cadre[[courbe]], 0.98), length.out = 200)",
  "  nd <- cadre[rep(1, length(grille)), , drop = FALSE]",
  "  nd[[courbe]] <- grille",
  "  Xp <- predict(g, nd, type = \"lpmatrix\")",
  "  js <- which(startsWith(colnames(Xp), sprintf(\"s(%s).\", courbe)))",
  "  set.seed(graine, kind = \"Mersenne-Twister\", normal.kind = \"Inversion\", sample.kind = \"Rejection\")",
  "  B <- MASS::mvrnorm(tirages, coef(g), vcov(g))",
  tx("  effet <- as.vector(Xp[, js, drop = FALSE] %*% coef(g)[js])  # (centrés : des écarts à la moyenne de la courbe)",
     "  effet <- as.vector(Xp[, js, drop = FALSE] %*% coef(g)[js])  # (centred: deviations from the curve's mean)"),
  "  simules <- Xp[, js, drop = FALSE] %*% t(B[, js, drop = FALSE])",
  "  effet <- effet - mean(effet)",
  "  simules <- sweep(simules, 2, colMeans(simules))",
  "  bande <- function(estimation, simulation) {",
  "    ecart_type <- apply(simulation, 1, sd)",
  "    critique <- quantile(apply(abs(simulation - estimation) / ecart_type, 2, max), niveau, names = FALSE)",
  "    list(bas = estimation - critique * ecart_type, haut = estimation + critique * ecart_type)",
  "  }",
  "  be <- bande(effet, simules)",
  "  x <- grille / poids + centre",
  "  h <- x[2] - x[1]",
  "  pente <- diff(effet) / h",
  "  bp <- bande(pente, apply(simules, 2, diff) / h)",
  "  x_pente <- (x[-1] + x[-length(x)]) / 2",
  "  plages <- function(mesure, etat, xs) {",
  "    r <- rle(etat)",
  "    fins <- cumsum(r$lengths)",
  "    debuts <- c(1, head(fins, -1) + 1)",
  "    nettes <- r$values != \"ns\"",
  "    data.frame(mesure = rep(mesure, sum(nettes)), sens = r$values[nettes], x_debut = xs[debuts][nettes], x_fin = xs[fins][nettes], row.names = NULL)",
  "  }",
  tx("  sens_pente <- ifelse(bp$bas > 0, \"hausse\", ifelse(bp$haut < 0, \"baisse\", \"ns\"))",
     "  sens_pente <- ifelse(bp$bas > 0, \"rise\", ifelse(bp$haut < 0, \"fall\", \"ns\"))"),
  tx("  sens_effet <- ifelse(be$bas > 0, \"au-dessus\", ifelse(be$haut < 0, \"au-dessous\", \"ns\"))",
     "  sens_effet <- ifelse(be$bas > 0, \"above\", ifelse(be$haut < 0, \"below\", \"ns\"))"),
  "  list(x = x, effet = effet, bas = be$bas, haut = be$haut, x_pente = x_pente, pente = pente, pente_bas = bp$bas, pente_haut = bp$haut,",
  tx("       plages = rbind(plages(\"pente\", sens_pente, x_pente), plages(\"effet\", sens_effet, x)))",
     "       plages = rbind(plages(\"slope\", sens_pente, x_pente), plages(\"effect\", sens_effet, x)))"),
  "}")

modele_r2_gamm <- function(tx) c(
  commentaire(tx("R² d'un modèle de mgcv::gamm (Nakagawa et Schielzeth, 2013) : marginal, la part de variance des effets fixes et des courbes (sa partie gam) ; conditionnel, avec les effets aléatoires `niveaux_aleatoires` de sa partie nlme (variances_niveaux ci-dessus, sur les lignes `tab`).",
                 "R² of an mgcv::gamm model (Nakagawa & Schielzeth, 2013): marginal, the share of variance of the fixed effects and curves (its gam part); conditional, with the random effects `random_levels` of its nlme part (level_variances above, over the rows `tab`).")),
  "r2_gamm <- function(m, niveaux_aleatoires, tab) {",
  "  fixes <- var(as.vector(fitted(m$gam)))",
  "  aleatoires <- sum(variances_niveaux(m$lme, niveaux_aleatoires, tab))",
  "  totale <- fixes + aleatoires + m$lme$sigma^2",
  "  c(marginal = fixes / totale, conditionnel = (fixes + aleatoires) / totale)",
  "}")

modele_predire_fixes <- function(tx) c(
  commentaire(tx("Les prédictions d'un modèle de mgcv sur l'échelle de la réponse, par ses seuls effets fixes : ses effets aléatoires (ses termes s(..., bs = \"re\"), ordonnées à l'origine et pentes) exclus, comme pour un groupe jamais vu ; chaque observation reçoit un groupe connu du modèle (`aleatoires`, leurs colonnes ; `appris` : ses données), que l'exclusion annule.",
                 "The predictions of an mgcv model on the scale of the response, by its fixed effects only: its random effects (its s(..., bs = \"re\") terms, intercepts and slopes) excluded, as for a group never seen; each observation gets a group known to the model (`random_terms`, their columns; `learnt`: its data), which the exclusion cancels.")),
  "predire_fixes <- function(m, nouvelles, appris, aleatoires) {",
  "  for (g in aleatoires) nouvelles[[g]] <- factor(rep(levels(appris[[g]])[1], nrow(nouvelles)), levels = levels(appris[[g]]))",
  "  exclus <- vapply(Filter(function(s) inherits(s, \"random.effect\"), m$smooth), function(s) s$label, \"\")",
  "  as.vector(predict(m, nouvelles, type = \"response\", exclude = exclus))",
  "}")

# Les résidus quantiles randomisés (regression_diagnostics.R : residus_quantiles), écrits pour le
# script : les mêmes tirages, de la même graine.
modele_residus_quantiles <- function(tx) c(
  commentaire(tx("Les résidus quantiles randomisés (Dunn et Smyth, 1996) : r = qnorm(u), u = F(y) pour une loi continue et, pour une loi discrète (`discrete`), tiré uniformément entre F(y − 1) et F(y) avec la graine `graine` ; `repartition(q, lower.tail)` : la fonction de répartition du modèle à la valeur ajustée de chaque observation. Ils suivent la loi normale quand le modèle est juste. Au-delà de 0,5, r se calcule par 1 − u (lower.tail = FALSE) : un comptage loin au-dessus de sa moyenne garde son résidu, au lieu de qnorm(1) = Inf.",
                 "The randomized quantile residuals (Dunn & Smyth, 1996): r = qnorm(u), u = F(y) for a continuous distribution and, for a discrete one (`discrete`), drawn uniformly between F(y − 1) and F(y) with the seed `seed`; `distribution_function(q, lower.tail)`: the model's distribution function at each observation's fitted value. They are normal when the model is right. Beyond 0.5, r is computed from 1 − u (lower.tail = FALSE): a count far above its mean keeps its residual, instead of qnorm(1) = Inf.")),
  "residus_quantiles <- function(y, repartition, discrete = TRUE, graine = 1) {",
  "  y0 <- if (discrete) y - 1 else y",
  "  F0 <- repartition(y0)                      # F(y - 1)",
  "  F1 <- repartition(y)                       # F(y)",
  "  S0 <- repartition(y0, lower.tail = FALSE)  # 1 - F(y - 1)",
  "  S1 <- repartition(y, lower.tail = FALSE)   # 1 - F(y)",
  "  v <- 0",
  "  if (discrete) {",
  "    set.seed(graine, kind = \"Mersenne-Twister\", normal.kind = \"Inversion\", sample.kind = \"Rejection\")",
  "    v <- runif(length(y))",
  "  }",
  "  u <- F0 + v * (F1 - F0)",
  "  ifelse(u <= 0.5, qnorm(u), qnorm(S0 - v * (S0 - S1), lower.tail = FALSE))",
  "}")

# Le test de permutation : dans un script d'un modèle, il réajuste ce modèle ; dans un script de
# séance, `ajuster(Yp)` donne l'ajustement et la validation croisée de chaque modèle.
modele_permutations <- function(tx, pl, na) {
  G <- if (!is.null(pl) && (pl$G != 7 || !is.null(pl$groupes_vc))) paste0(", ", arguments_groupes(pl)) else ""
  mesure <- if (is.null(pl)) c("    r <- ajuster(Yp)", "    A <- ncol(r[[1]]$T)", "    m <- r[[1]]", "    vc <- r[[2]]")
  else c("    s <- pretraiter(X, Yp, seq_len(nrow(X)))", switch(pl$type,
    pls = c("    m <- pls(s$X, s$Y, A)", sprintf("    vc <- validation_croisee_pls(X, Yp, A, pretraiter%s)", G)),
    opls = if (pl$Ap > 1) c("    m <- opls(s$X, s$Y, Ap, Ao)", sprintf("    vc <- validation_croisee_opls(X, Yp, Ap, Ao, pretraiter%s)", G), "    A <- Ap + Ao")
           else c("    m <- opls(s$X, s$Y, Ao)", sprintf("    vc <- validation_croisee_opls(X, Yp, Ao, pretraiter%s)", G), "    A <- 1 + Ao")))
  arguments <- if (is.null(pl)) "ajuster, Y" else switch(pl$type, pls = "X, Y, A", opls = if (pl$Ap > 1) "X, Y, Ap, Ao" else "X, Y, Ao")
  permuter <- if (is.null(pl)) c("    Yp <- Y", "    if (classes) Yp <- Y[ordre, , drop = FALSE] else Yp[, j] <- Y[ordre, j]")
              else if (pl$plsda) tx("    Yp <- Y[ordre, , drop = FALSE]  # les classes : toutes les indicatrices ensemble",
                                    "    Yp <- Y[ordre, , drop = FALSE]  # the classes: all the indicators together")
              else c("    Yp <- Y", "    Yp[, j] <- Y[ordre, j]")
  c(commentaire(c(tx("Test de permutation : les valeurs du Y numéro j sont permutées au hasard entre les observations, n fois, et chaque fois tout le modèle est réajusté (même nombre de composantes), prétraitement et validation croisée compris. R2 et Q2 de ce Y en fonction de la corrélation entre le Y d'origine et le Y permuté : les ordonnées à l'origine de leurs droites de régression mesurent le surajustement (règle usuelle : Q2 < 0,05 et R2 < 0,3 à 0,4 pour un modèle valide).",
                     "Permutation test: the values of Y number j are permuted at random among the observations, n times, and each time the whole model is refitted (same number of components), preprocessing and cross-validation included. R2 and Q2 of this Y against the correlation between the original and the permuted Y: the intercepts of their regression lines measure overfitting (usual rule: Q2 < 0.05 and R2 < 0.3 to 0.4 for a valid model)."),
                  if (is.null(pl)) tx("`ajuster(Yp)` : l'ajustement et la validation croisée du modèle avec Yp ; `classes` : Y est fait des indicatrices des classes d'une PLS-DA, permutées ensemble.",
                                      "`fit(Yp)`: the fit and cross-validation of the model with Yp; `classes`: Y is made of the class indicators of a PLS-DA, permuted together."))),
    sprintf("permutations <- function(%s, n = 20, j = 1%s) {", arguments, if (is.null(pl)) ", classes = FALSE" else ""),
    "  mesure <- function(Yp) {",
    mesure,
    "    c(R2 = 1 - unname(m$ssy[j, A + 1] / m$ssy[j, 1]), Q2 = 1 - unname(vc$press[A, j] / vc$ss[1, j]))",
    "  }",
    "  set.seed(1)",
    "  points <- data.frame(correlation = 1, t(mesure(Y)))",
    "  for (i in 1:n) {",
    "    ordre <- sample(nrow(Y))",
    permuter,
    sprintf("    points <- rbind(points, data.frame(correlation = abs(cor(Y[, j], Yp[, j]%s)), t(mesure(Yp))))", if (na) ", use = \"complete.obs\"" else ""),
    "  }",
    "  list(points = points, ordonnee_R2 = coef(lm(R2 ~ correlation, points))[[1]],",
    "       ordonnee_Q2 = coef(lm(Q2 ~ correlation, points))[[1]])",
    "}")
}

modele_snv <- function(tx) c(
  commentaire(tx("SNV : chaque spectre (ligne) centré sur sa moyenne et divisé par son écart-type.", "SNV: each spectrum (row) centred on its mean and divided by its standard deviation.")),
  "snv <- function(S) {",
  "  moyenne <- rowMeans(S, na.rm = TRUE)",
  "  ecart <- apply(S, 1, sd, na.rm = TRUE)",
  "  ecart[!is.finite(ecart) | ecart == 0] <- NA",
  "  (S - moyenne) / ecart",
  "}")

modele_msc <- function(tx) c(
  commentaire(tx("MSC : chaque spectre x ajusté sur le spectre de référence r, x = a + b r, puis remplacé par (x - a) / b.",
                 "MSC: each spectrum x fitted on the reference spectrum r, x = a + b r, then replaced by (x - a) / b.")),
  "msc <- function(S, r) {",
  "  t(apply(S, 1, function(x) {",
  "    ok <- !is.na(x) & !is.na(r)",
  "    if (sum(ok) < 2 || var(r[ok]) == 0) return(rep(NA_real_, length(x)))",
  "    b <- cov(x[ok], r[ok]) / var(r[ok])",
  "    a <- mean(x[ok]) - b * mean(r[ok])",
  "    (x - a) / b",
  "  }))",
  "}")

modele_savitzky_golay <- function(tx) c(
  commentaire(tx("Savitzky–Golay : dans une fenêtre de variables centrée sur chaque variable, un polynôme de degré `degre` ajusté par moindres carrés ; la valeur corrigée est sa valeur (derivee = 0) ou sa dérivée au centre. Aux bords, le polynôme de la première ou de la dernière fenêtre. coefficients_sg : les coefficients qui donnent cette valeur à la position u de la fenêtre (0 : son centre).",
                 "Savitzky–Golay: in a window of variables centred on each variable, a polynomial of degree `degree` fitted by least squares; the corrected value is its value (derivative = 0) or its derivative at the centre. At the edges, the polynomial of the first or last window. sg_coefficients: the coefficients that give this value at position u of the window (0: its centre).")),
  "coefficients_sg <- function(fenetre, degre, derivee, u = 0) {",
  "  h <- (fenetre - 1) / 2",
  "  V <- outer(-h:h, 0:degre, `^`)",
  "  H <- solve(crossprod(V), t(V))",
  "  d <- vapply(0:degre, function(k) if (k < derivee) 0 else factorial(k) / factorial(k - derivee) * u^(k - derivee), 0)",
  "  drop(d %*% H)",
  "}",
  "savitzky_golay <- function(S, fenetre, degre, derivee) {",
  "  K <- ncol(S)",
  "  h <- (fenetre - 1) / 2",
  "  corrige <- matrix(NA_real_, nrow(S), K, dimnames = dimnames(S))",
  "  centre <- coefficients_sg(fenetre, degre, derivee)",
  "  for (j in (h + 1):(K - h)) corrige[, j] <- S[, (j - h):(j + h), drop = FALSE] %*% centre",
  "  for (j in seq_len(h)) {",
  "    corrige[, j] <- S[, 1:fenetre, drop = FALSE] %*% coefficients_sg(fenetre, degre, derivee, j - h - 1)",
  "    corrige[, K - h + j] <- S[, (K - fenetre + 1):K, drop = FALSE] %*% coefficients_sg(fenetre, degre, derivee, j)",
  "  }",
  "  corrige",
  "}")

modele_detrend <- function(tx, na) c(
  commentaire(tx(sprintf("detrend : chaque spectre moins le polynôme de degré `degre` de l'abscisse x qui lui est ajusté par moindres carrés%s.", if (na) " (sur ses valeurs présentes)" else ""),
                 sprintf("detrend: each spectrum minus the polynomial of degree `degree` of the abscissa x fitted to it by least squares%s.", if (na) " (over its present values)" else ""))),
  "detrend <- function(S, x, degre) {",
  "  u <- (x - mean(x)) / sd(x)",
  "  V <- outer(u, 0:degre, `^`)",
  "  complet <- qr(V)",
  if (na) c("  for (i in seq_len(nrow(S))) {",
            "    ok <- !is.na(S[i, ])",
            "    if (all(ok)) S[i, ] <- qr.resid(complet, S[i, ])",
            "    else if (sum(ok) > degre) S[i, ok] <- qr.resid(qr(V[ok, , drop = FALSE]), S[i, ok])",
            "    else S[i, ] <- NA",
            "  }")
  else "  for (i in seq_len(nrow(S))) S[i, ] <- qr.resid(complet, S[i, ])",
  "  S",
  "}")

modele_ligne_base <- function(tx) c(
  commentaire(tx("Ligne de base : chaque spectre moins son enveloppe convexe inférieure (les segments qui joignent ses points les plus bas), interpolée linéairement ; enveloppe_basse : les points de cette enveloppe (chaîne monotone d'Andrew), x croissant.",
                 "Baseline: each spectrum minus its lower convex hull (the segments joining its lowest points), linearly interpolated; lower_hull: the points of this hull (Andrew's monotone chain), x increasing.")),
  "ligne_base <- function(S, x) {",
  "  o <- order(x)",
  "  for (i in seq_len(nrow(S))) {",
  "    ok <- o[!is.na(S[i, o])]",
  "    if (length(ok) < 2) next",
  "    h <- enveloppe_basse(x[ok], S[i, ok])",
  "    S[i, ok] <- S[i, ok] - approx(x[ok][h], S[i, ok][h], xout = x[ok])$y",
  "  }",
  "  S",
  "}",
  "enveloppe_basse <- function(x, y) {",
  "  h <- integer(0)",
  "  for (i in seq_along(x)) {",
  "    while (length(h) >= 2) {",
  "      j <- h[length(h) - 1]",
  "      k <- h[length(h)]",
  "      if ((x[k] - x[j]) * (y[i] - y[j]) - (y[k] - y[j]) * (x[i] - x[j]) > 0) break",
  "      h <- h[-length(h)]",
  "    }",
  "    h <- c(h, i)",
  "  }",
  "  h",
  "}")

# ---------------------------------------------------------------- la langue du script

# Les noms du script en anglais : le code est écrit avec des noms français, puis, pour un script
# en anglais, ses noms (pas ses textes ni ses commentaires, écrits dans les deux langues) sont
# remplacés d'après cette table. Les noms venus des données sont toujours dans des chaînes.
NOMS_SCRIPT_EN <- c(
  residus_normalises_car1 = "car1_normalized_residuals",
  temps_en_nombres = "times_as_numbers", morceaux = "parts", motif = "pattern", numeros = "group_numbers", debut = "first", fin = "last",
  heure = "time_of_day", jma = "day_first", bissextile = "leap", jour_annee = "day_of_year",
  donnees = "data", lignes = "rows", lignes_test = "test_rows", modele = "in_model", y_observe = "y_observed",
  cochees = "ticked", donnees_texte = "data_as_text", valeur_decalee = "shifted_value", lire_temps = "read_time",
  centre_x = "centre_x", poids_x = "weight_x", ecart_x = "sd_x", centre_y = "centre_y", poids_y = "weight_y", ecart_y = "sd_y",
  centre_c = "centre_c", poids_c = "weight_c", ecart_c = "sd_c", centre_cy = "centre_cy", poids_cy = "weight_cy", ecart_cy = "sd_cy",
  croises = "cross", croises_y = "cross_y", ordre_x = "order_x", ordre_y = "order_y", spectre = "spectrum", abscisses = "abscissas",
  zone = "zone", projeter = "project", composante_acp = "pca_component", composante_pls = "pls_component",
  composantes_pls = "pls_components", base_poids_y = "y_weight_basis", acp = "pca", opls_noyau = "opls_core",
  opls_predire = "opls_predict", validation_croisee_acp = "cross_validation_pca", validation_croisee_pls = "cross_validation_pls",
  validation_croisee_opls = "cross_validation_opls", mettre_a_echelle = "scale_columns", nommer = "number_columns",
  prefixe = "prefix", la_plus_grande = "largest", coefficients_sg = "sg_coefficients", ligne_base = "baseline",
  enveloppe_basse = "lower_hull", fenetre = "window", degre = "degree", derivee = "derivative", corrige = "corrected",
  moyenne = "means", ecart = "sds", complet = "full", centre = "centre", poids = "weight", t_suivant = "t_next",
  u_suivant = "u_next", fini = "done", groupe = "group", groupe_terme = "term_group", hors = "out", reste = "remaining",
  colonne = "column", cle = "key", predictives = "predictive", opls = "opls", mesure = "measure",
  ordonnee_R2 = "intercept_R2", ordonnee_Q2 = "intercept_Q2", ajuster = "fit", rapport = "ratio", depart = "start",
  composantes = "components", composante = "component", valeur_propre = "eigenvalue", notes = "scored",
  correction = "correction", pouvoir = "modelling_power", DModX_pondere = "DModX_weighted", part_y = "y_share",
  Constante = "Constant", y_ajuste = "y_fitted", ss_residus = "ss_residual", dl = "df", classe = "class_observed",
  classe_vc = "class_cv", classe_predite = "class_predicted", observee = "observed", predite = "predicted",
  limites = "limits", terme = "term", signes = "signs", hors_domaine = "outside_domain", y_predit = "y_predicted",
  erreur = "error", biais = "bias", predictions_test = "test_predictions", classification_test = "classification_test",
  couleur = "colour", ordre = "ranking", pretraiter = "preprocess", apprentissage = "training", cases = "cells",
  groupes_vc = "cv_groups",
  # les régressions
  termes_utiles = "useful_terms", termes = "model_terms", garde = "kept_columns", aic_corrige = "aic_corrected",
  validation_croisee = "cross_validation", cadre = "model_data", predire = "predict_group", echecs = "failures", ici = "here",
  appris = "learnt", r2_mixte = "r2_mixed", fixes = "fixed", aleatoires = "random_terms", totale = "total",
  conditionnel = "conditional", dimension_courbe = "basis_dimension", predire_fixes = "predict_fixed", nouvelles = "new_rows",
  reponse = "response", noms_termes = "term_labels", connu = "known", tableau_modele = "model_table", noms_obs = "obs_names",
  noms_test = "test_names", cadre_test = "test_data", retenues = "complete_rows", aleatoire = "random_part",
  ajuster_modele = "fit_model", candidats = "candidates", methode = "method", utiles = "useful", tenter = "attempt",
  reglages = "settings", lis = "smooths", famille = "distribution", lisses = "smooth_terms", niveaux_aleatoires = "random_levels", k_demandes = "k_requested", penalise = "penalized",
  estimation = "estimate", erreur_type = "std_error", ddl = "df", bas = "lower", haut = "upper",
  effet_multiplicatif = "multiplicative_effect", rapport_cotes = "odds_ratio", r2_ajuste = "r2_adjusted",
  deviance_expliquee = "deviance_explained", criteres = "criteria", parametres = "parameters", echelle_estimee = "scale_estimated",
  courbes = "curves", ref_ddl = "ref_df", statistique = "statistic", concurvite = "concurvity", concurvite_pire = "concurvity_worst",
  seuils_courbe = "curve_thresholds", seuils = "thresholds", plages_seuils = "threshold_ranges", grille = "grid", tirages = "draws",
  niveau = "level", graine = "seed", simules = "simulated", bande = "band", simulation = "simulation",
  ecart_type = "std_dev", critique = "critical", pente = "slope", pente_bas = "slope_lower", pente_haut = "slope_upper",
  x_pente = "x_slope", plages = "ranges", etat = "state", fins = "ends", debuts = "starts", nettes = "clear", sens = "direction",
  sens_pente = "slope_direction", sens_effet = "effect_direction", x_debut = "x_start", x_fin = "x_end", be = "effect_band", bp = "slope_band", ecart_types = "std_devs", parametre_loi = "distribution_parameter",
  observe = "observed", ajuste = "fitted", residu = "residual", ajuste_fixe = "fitted_fixed", predit = "predicted",
  residus_quantiles = "quantile_residuals", residu_quantile = "quantile_residual", repartition = "distribution_function",
  courbe = "curve", abscisse = "abscissa", effet = "effect", droite = "rhs",
  selection_aic = "aic_selection", groupes_aic = "aic_groups", requis_aic = "aic_required", criteres_aic = "aic_criteria", choisis = "chosen",
  permise = "allowed", combinaisons = "combinations", combinaison = "combination", valeurs_aic = "aic_values", etapes_aic = "aic_steps",
  courant = "current", retirables = "removable", essais = "trials", aicc_courant = "current_aicc", modeles_aic = "aic_models",
  importance_aic = "aic_importance", poids_cumule = "cumulative_weight", etape = "step", toutes = "all_subsets", termes_aic = "aic_terms",
  modeles = "models", ajuster_ml = "fit_ml", libelle_aic = "aic_label", contient_aic = "aic_contains", hors_selection = "outside_selection",
  termes_candidats = "candidate_terms", pentes_seules = "level_slopes", niveaux_inconnus = "unseen_levels", pentes = "slopes", sans = "unseen",
  variances_niveaux = "level_variances", variances_pentes = "slope_variances", noms_niveaux = "level_labels", nom_residuelle = "residual_label",
  pentes_aleatoires = "random_slopes", termes_gardes = "kept_terms", exclus = "excluded")

nom_script <- function(x, langue) if (identical(langue, "en")) ifelse(x %in% names(NOMS_SCRIPT_EN), NOMS_SCRIPT_EN[x], x) else x

# Le code avec ses noms dans la langue du script (voir NOMS_SCRIPT_EN).
traduire_code <- function(lignes, langue) {
  if (!identical(langue, "en") || !length(lignes)) return(lignes)
  lignes <- unlist(lapply(lignes, function(l) if (grepl("\n", l, fixed = TRUE)) strsplit(l, "\n", fixed = TRUE)[[1]] else l))
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"), includeText = TRUE)
  pd <- pd[pd$terminal & pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB", "SYMBOL_FORMALS") & pd$text %in% names(NOMS_SCRIPT_EN), ]
  pd <- pd[order(pd$line1, -pd$col1), ]
  for (i in seq_len(nrow(pd))) {
    l <- lignes[pd$line1[i]]
    lignes[pd$line1[i]] <- paste0(substr(l, 1, pd$col1[i] - 1), NOMS_SCRIPT_EN[[pd$text[i]]], substr(l, pd$col2[i] + 1, nchar(l)))
  }
  lignes
}

# Les noms qu'utilise du code R (pour ne pas leur donner le nom d'un modèle).
symboles_code <- function(lignes) {
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"))
  unique(pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS")])
}

# ---------------------------------------------------------------- l'écart avec Nuée

# Écart entre les résultats du script (dans `e`) et ceux de `m`, le modèle de l'application :
# NULL s'il n'y en a pas, sinon les écarts relatifs de ce qui diffère (R2 et Q2, scores, T2,
# DModX, limites, VIP, coefficients, CV-ANOVA, classification, jeu de test).
ecart_script <- function(pl, m, e, langue) {
  r <- function(x) get0(nom_script(x, langue), envir = e, inherits = FALSE)
  s <- m$statistiques
  ms <- r("m")
  if (!identical(dim(ms$T), dim(m$T))) {
    return(sprintf(traduire("%d composantes et %d observations au lieu de %d et %d", "%d components and %d observations instead of %d and %d"),
                   NCOL(ms$T), NROW(ms$T), ncol(m$T), nrow(m$T)))
  }
  pls <- pl$type != "acp"
  ny <- m$y$noms
  colonnes_cv <- c("SS", "DF", "MS", "F", "p")
  # X entièrement décrit par le modèle : ses résidus ne sont que des erreurs d'arrondi, et DModX
  # (leur écart-type rapporté au leur) n'a pas de sens, ici comme dans le moteur
  bruit <- m$ssx[m$A + 1] < 1e-20 * m$ssx[1]
  paires <- list(
    "R2/Q2" = list(s$composantes, r("composantes")), scores = list(m$T, ms$T), T2 = list(s$observations$T2, r("T2")),
    DModX = if (!bruit) list(s$observations$DModX, r("DModX")), "DModX (pond.)" = if (!bruit) list(s$observations$DModX_pondere, r("DModX_pondere")),
    limites = list(unlist(s$limites), r("limites")),
    VIP = list(s$variables$VIP, r("VIP")),
    coefficients = if (pls) list(as.matrix(s$coefficients[paste("Coeff", ny)]), r("coefficients")),
    "CV-ANOVA" = if (pl$cv_anova) list(do.call(rbind, lapply(s$cv_anova, `[`, colonnes_cv)), do.call(rbind, lapply(r("cv_anova"), `[`, colonnes_cv))),
    classification = if (pl$plsda) list(s$classification$vc$table, r("classification")),
    test = if (!is.null(m$test$statistiques) && !pl$plsda) list(m$test$statistiques[c("n", "RMSEP", "biais", "SEP", "Q2ext")], r("test")[-1]),
    "test (domaine)" = if (!is.null(m$test)) list(m$test$hors_domaine, sum(r("predictions_test")[[nom_script("hors_domaine", langue)]])),
    "test (classes)" = if (!is.null(m$test$classement)) list(m$test$classement$table, r("classification_test")))
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

# Écart relatif entre deux tableaux de nombres (mêmes dimensions, mêmes valeurs manquantes).
ecart_valeurs <- function(a, b) {
  if (is.null(a) && is.null(b)) return(0)
  if (is.null(a) || is.null(b)) return(Inf)
  nombres <- function(x) {
    x <- as.matrix(if (is.table(x)) unclass(x) else x)
    dimnames(x) <- NULL
    storage.mode(x) <- "double"
    x
  }
  a <- nombres(a)
  b <- nombres(b)
  if (!identical(dim(a), dim(b)) || !identical(is.na(a), is.na(b))) return(Inf)
  finis <- is.finite(a)
  if (!identical(finis, is.finite(b)) || !identical(a[!finis & !is.na(a)], b[!finis & !is.na(b)])) return(Inf)
  if (!any(finis)) return(0)
  max(abs(a[finis] - b[finis])) / max(abs(a[finis]), 1e-12)
}

# ---------------------------------------------------------------- les autres parties

# Un texte venu des données (nom, titre) écrit hors d'une chaîne R : dans un commentaire du
# script, ou dans le texte d'un rapport R Markdown ou Quarto. Un saut de ligne en sortirait (la
# suite deviendrait du code), et dans un rapport knitr exécute ce qui est écrit `r ainsi` : les
# sauts de ligne deviennent des espaces, les accents graves des apostrophes.
texte_sur <- function(x) gsub("`", "'", gsub("[\r\n\u2028\u2029]+", " ", x))

# Un commentaire de script, coupé en lignes d'au plus `largeur` caractères. Les signes précédés
# d'une espace (« ; », « : », « ? », « ! », « » ») restent avec le mot d'avant.
commentaire <- function(texte, retrait = "", largeur = 98) {
  if (!length(texte)) return(character(0))
  mots <- strsplit(paste(texte, collapse = " "), " ", fixed = TRUE)[[1]]
  mots <- mots[nzchar(mots)]
  unites <- character(0)
  for (w in mots) {
    n <- length(unites)
    if (n && (w %in% c(";", ":", "?", "!") || startsWith(w, "»") || unites[n] == "«")) unites[n] <- paste(unites[n], w) else unites <- c(unites, w)
  }
  lignes <- character(0)
  ligne <- ""
  for (u in unites) {
    if (nzchar(ligne) && nchar(ligne) + 1 + nchar(u) > largeur - nchar(retrait) - 2) {
      lignes <- c(lignes, ligne)
      ligne <- u
    } else ligne <- if (nzchar(ligne)) paste(ligne, u) else u
  }
  paste0(retrait, "# ", c(lignes, ligne))
}

liste_textes <- function(x, et) if (length(x) <= 1) paste(x) else paste(paste(x[-length(x)], collapse = ", "), et, x[length(x)])

type_script <- function(d, langue = "fr") {
  type <- if (!is.null(d$type) && d$type %in% c("acp", "pls", "opls")) d$type else if (isTRUE(d$opls)) "opls" else if (identical(d$type, "PCA")) "acp" else "pls"
  paste0(if (type == "opls") "O" else "", if (isTRUE(d$plsda)) "PLS-DA" else if (type == "acp") (if (identical(langue, "en")) "PCA" else "ACP") else "PLS")
}

texte_graphiques <- function(langue) if (identical(langue, "en")) "# Plots (base R)" else "# Graphiques (R de base)"

titre_partie <- function(k, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  sprintf("# %d. %s ----", k, switch(k, tx("Fonctions", "Functions"), tx("Données", "Data"), tx("Modèle", "Model"), tx("Résultats", "Results")))
}

# Ce qui est à côté du script, dans son en-tête.
fichiers_a_cote <- function(fichiers, langue, modeles) {
  tx <- function(fr, en) if (langue == "en") en else fr
  autres <- fichiers[names(fichiers) != "script"]
  if (!length(autres)) return(character(0))
  quoi <- c(donnees = tx("les données (le script les lit)", "the data (the script reads them)"),
            rdata = if (modeles) tx("les données et les modèles ajustés : load() les rend sans recalculer", "the data and the fitted models: load() gives them back without computing")
                    else tx("les données et le modèle ajusté : load() les rend sans recalculer", "the data and the fitted model: load() gives them back without computing"),
            renv = tx("les versions de R et des paquets : renv::restore() installe les mêmes", "the versions of R and the packages: renv::restore() installs the same"),
            rproj = tx("le projet RStudio : l'ouvrir ouvre ce dossier", "the RStudio project: opening it opens this folder"))
  c(tx("# À côté de ce script :", "# Next to this script:"), sprintf("#   %s : %s", basename(autres), quoi[names(autres)]))
}

entete_script <- function(pl, nom_projet, version, fichiers, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  A <- pl$A
  c(sprintf(tx("# Modèle %s%s : %s, %d composante%s, %d observations, %d termes X", "# Model %s%s: %s, %d component%s, %d observations, %d X terms"),
            texte_sur(pl$nom), if (nzchar(pl$titre)) sprintf(" (%s)", texte_sur(pl$titre)) else "", type_script(pl, langue), A, if (A > 1) "s" else "", pl$N, pl$K),
    sprintf(tx("# Script R écrit par Nuée %s le %s, à partir du projet « %s ».", "# R script written by Nuée %s on %s, from the project “%s”."), version, format(Sys.Date()), texte_sur(nom_projet)),
    tx("# Il refait ce modèle avec R seul, sans Nuée ni autre paquet : le calcul est écrit pour ce modèle,",
       "# It redoes this model with R alone, without Nuée or any package: the computation is written for this"),
    tx("# de ses données à ses résultats, qui sont ceux de Nuée. Ses quatre parties :",
       "# model, from its data to its results, which are those of Nuée. Its four parts:"),
    tx("#   1. Fonctions : l'algorithme NIPALS, la validation croisée", "#   1. Functions: the NIPALS algorithm, the cross-validation"),
    tx("#   2. Données", "#   2. Data"),
    tx("#   3. Modèle : termes X et Y, mise à l'échelle, ajustement, statistiques", "#   3. Model: X and Y terms, scaling, fit, statistics"),
    tx("#   4. Résultats : tableaux et graphiques", "#   4. Results: tables and plots"),
    tx("# Dans RStudio, Ctrl+Maj+O affiche ces parties ; Ctrl+Maj+S lance tout le script.", "# In RStudio, Ctrl+Shift+O shows these parts; Ctrl+Shift+S runs the whole script."),
    sprintf(tx("# Écrit avec R %s ; il n'utilise que R (stats et graphics sont livrés avec R).", "# Written with R %s; it uses R alone (stats and graphics come with R)."),
            paste(R.version$major, R.version$minor, sep = ".")),
    fichiers_a_cote(fichiers, langue, FALSE))
}

entete_seance <- function(plans, noms, nom_projet, version, fichiers, langue, paquets = character(0)) {
  tx <- function(fr, en) if (langue == "en") en else fr
  modele <- function(k) {
    pl <- plans[[k]]
    titre <- if (nzchar(pl$titre)) sprintf(" (%s)", texte_sur(pl$titre)) else ""
    if (isTRUE(pl$regression)) {
      return(sprintf(tx("#   %s%s : %s, %d observations, %d termes X", "#   %s%s: %s, %d observations, %d X terms"), noms[k], titre, type_regression(pl, langue), pl$N, pl$K))
    }
    sprintf(tx("#   %s%s : %s, %d composante%s, %d observations, %d termes X", "#   %s%s: %s, %d component%s, %d observations, %d X terms"),
            noms[k], titre, type_script(pl, langue), pl$A, if (pl$A > 1) "s" else "", pl$N, pl$K)
  }
  c(sprintf(tx("# Les %d modèles de la séance du projet « %s » :", "# The %d models of the session of the project “%s”:"), length(noms), texte_sur(nom_projet)),
    vapply(seq_along(noms), modele, ""),
    sprintf(tx("# Script R écrit par Nuée %s le %s.", "# R script written by Nuée %s on %s."), version, format(Sys.Date())),
    if (length(paquets)) {
      commentaire(sprintf(tx("Il refait ces modèles avec %s, sans Nuée : le calcul est écrit pour eux, de leurs données à leurs résultats, qui sont ceux de Nuée. Ses parties :",
                             "It redoes these models with %s, without Nuée: the computation is written for them, from their data to their results, which are those of Nuée. Its parts:"),
                          texte_paquets(paquets, langue)))
    } else {
      c(tx("# Il refait ces modèles avec R seul, sans Nuée ni autre paquet : le calcul est écrit pour eux, de",
           "# It redoes these models with R alone, without Nuée or any package: the computation is written for"),
        tx("# leurs données à leurs résultats, qui sont ceux de Nuée. Ses parties :", "# them, from their data to their results, which are those of Nuée. Its parts:"))
    },
    tx("#   1. Fonctions : l'algorithme NIPALS, la validation croisée", "#   1. Functions: the NIPALS algorithm, the cross-validation"),
    tx("#   2. Données", "#   2. Data"),
    tx("#   3. Modèles et résultats : un bloc par modèle, ses résultats gardés dans une liste à son nom",
       "#   3. Models and results: one block per model, its results kept in a list under its name"),
    tx("# Dans RStudio, Ctrl+Maj+O affiche ces parties ; Ctrl+Maj+S lance tout le script.", "# In RStudio, Ctrl+Shift+O shows these parts; Ctrl+Shift+S runs the whole script."),
    if (length(paquets)) sprintf(tx("# Écrit avec %s.", "# Written with %s."), texte_paquets(paquets, langue, versions = TRUE))
    else sprintf(tx("# Écrit avec R %s ; il n'utilise que R (stats et graphics sont livrés avec R).", "# Written with R %s; it uses R alone (stats and graphics come with R)."),
                 paste(R.version$major, R.version$minor, sep = ".")),
    fichiers_a_cote(fichiers, langue, TRUE))
}

# Le rapport R Markdown ou Quarto : les méthodes, les données, le code du modèle (replié dans le
# rapport, une fois compilé), ses résultats (tableaux et graphiques en R de base) et les
# références.
lignes_rapport <- function(format, pl, langue, methodes, fonctions, code_donnees, x) {
  tx <- function(fr, en) if (langue == "en") en else fr
  bloc <- function(options, code) c(sprintf("```{r%s}", if (nzchar(options)) paste0(" ", options) else ""), code, "```", "")
  kable <- function(objet, chiffres) traduire_code(sprintf("knitr::kable(%s%s)", objet, if (is.na(chiffres)) "" else sprintf(", digits = %d", chiffres)), langue)
  titre <- sprintf("%s %s%s", tx("Modèle", "Model"), pl$nom, if (nzchar(pl$titre)) paste(" —", pl$titre) else "")
  yaml <- c("---", sprintf("title: %s", code_texte(titre)), sprintf("date: %s", code_texte(format(Sys.Date()))), sprintf("lang: %s", langue),
            if (format == "Rmd") c("output:", "  html_document:", "    code_folding: hide") else c("format:", "  html:", "    embed-resources: true", "    code-fold: true"),
            "---", "")
  test <- length(pl$rangs_test) > 0
  pls <- pl$type != "acp"
  graphiques <- unlist(lapply(x$graphiques, function(g) bloc(sprintf("%s, fig.width = %s, fig.height = %s", g$nom, g$largeur, g$hauteur), g$code)))
  resultats <- if (isTRUE(pl$regression)) {
    c(bloc("coefficients", kable("coefficients", 4)),
      bloc("criteres", kable("t(criteres)", 2)),
      bloc("qualite", kable("t(c(r2, Q2 = vc$Q2, RMSEcv = vc$RMSEcv))", 3)),
      if (length(pl$niveaux)) bloc("variances", kable("variances", 4)),
      # (« courbes » est le bloc de leurs graphiques : knitr refuse deux blocs du même nom)
      if (length(pl$lisses)) bloc("courbes-edf", kable("courbes", 3)),
      if (length(pl$lisses)) bloc("plages-seuils", kable("plages_seuils", 3)),
      if (!is.null(pl$selection)) bloc("selection-aic", kable("head(selection$modeles, 20)", 3)),
      if (!is.null(pl$selection)) bloc("importance-aic", kable("selection$importance", 3)),
      graphiques,
      if (test) bloc("test", kable("test", 3)))
  } else {
    c(bloc("composantes", kable("composantes", 3)),
      graphiques,
      bloc("limites", kable("t(limites)", 3)),
      if (pl$cv_anova) bloc("cv-anova", kable("cv_anova[[1]]", 4)),
      if (pl$plsda) bloc("classification", kable("classification", NA)),
      if (test && pls && !pl$plsda) bloc("test", kable("test", 3)),
      if (test && pl$plsda) bloc("test", kable("classification_test", NA)))
  }
  c(yaml,
    sprintf("## %s", tx("Méthodes", "Methods")), "", rbind(texte_sur(methodes$paragraphes), ""),
    sprintf("## %s", tx("Données", "Data")), "",
    bloc("donnees, echo = FALSE", code_donnees),
    bloc("", traduire_code("str(donnees, list.len = 12)", langue)),
    sprintf("## %s", tx("Modèle", "Model")), "",
    tx("Le calcul, écrit en R pour ce modèle (bouton « Code » pour le voir).", "The computation, written in R for this model (“Code” button to see it)."), "",
    bloc("fonctions", fonctions[seq_len(max(0, length(fonctions) - 1))]),
    bloc("modele", x$calcul),
    sprintf("## %s", tx("Résultats", "Results")), "",
    resultats,
    sprintf("## %s", tx("Références", "References")), "", paste("-", methodes$references))
}

# Le script et, au choix, les fichiers à côté : données en CSV, .RData, renv.lock, .Rproj.
fichiers_script <- function(chemin_script, donnees, rdata, renv, rproj, langue = "fr") {
  base <- sub("\\.[^.]+$", "", basename(chemin_script))
  dossier <- dirname(chemin_script)
  c(script = chemin_script,
    donnees = if (donnees == "csv") file.path(dossier, paste0(base, if (identical(langue, "en")) " - data.csv" else " - donnees.csv")),
    rdata = if (rdata) file.path(dossier, paste0(base, ".RData")),
    renv = if (renv) file.path(dossier, "renv.lock"),
    rproj = if (rproj) file.path(dossier, paste0(base, ".Rproj")))
}

# Les données dans le script, ou le CSV écrit à côté et le code qui le lit.
donnees_script <- function(tab, fichiers, langue) {
  nom <- nom_script("donnees", langue)
  if (!"donnees" %in% names(fichiers)) return(code_donnees(tab$donnees, nom))
  ecrire_csv_script(tab$donnees, fichiers[["donnees"]])
  code_lire_csv(tab$donnees, basename(fichiers[["donnees"]]), langue, nom)
}

# Écrit le script et les fichiers d'à côté ; `objets` : ce que le .RData garde de `e` (ses
# fonctions, comme pretraiter(), y sont rattachées à l'environnement global de la session qui le
# relira, et non à `e`, où le script a tourné ici).
ecrire_fichiers_script <- function(lignes, fichiers, e, objets, paquets) {
  writeBin(charToRaw(enc2utf8(paste0(lignes, "\n", collapse = ""))), fichiers[["script"]])
  if ("rdata" %in% names(fichiers)) {
    global <- function(x) {
      if (is.function(x)) environment(x) <- globalenv()
      else if (is.list(x) && !is.data.frame(x)) x[] <- lapply(x, global)
      x
    }
    garde <- lapply(mget(intersect(objets, ls(e)), envir = e), global)
    save(list = names(garde), envir = list2env(garde), file = fichiers[["rdata"]])
  }
  if ("renv" %in% names(fichiers)) ecrire_renv(paquets, fichiers[["renv"]])
  if ("rproj" %in% names(fichiers)) writeBin(charToRaw(paste0(c("Version: 1.0", "", "RestoreWorkspace: No", "SaveWorkspace: No", "AlwaysSaveHistory: Default", "",
                                                               "EnableCodeIndexing: Yes", "UseSpacesForTab: Yes", "NumSpacesForTab: 2", "Encoding: UTF-8"), "\n", collapse = "")),
                                             fichiers[["rproj"]])
}

# Le tableau du script : les noms des observations, puis une colonne par variable (des facteurs
# pour les variables qualitatives, avec toutes leurs catégories, dans leur ordre), puis les
# identifiants des observations `ids` dont une régression fait ses groupes (ids_script). Les noms
# des colonnes sont rendus uniques ; `noms` les donne par numéro de variable du projet, `ids` par
# nom d'identifiant, `rangs` le rang de chaque observation (sa ligne dans le tableau).
tableau_script <- function(projet, ids = character(0)) {
  obs <- rownames(projet$donnees)
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(projet$donnees), projet$variables)
  noms <- vapply(vars, function(v) as.character(v$ids[1] %||% ""), "")
  noms[!nzchar(noms)] <- paste0("V", which(!nzchar(noms)))
  ids <- unique(as.character(ids))
  noms <- make.unique(c("Observation", noms, ids), sep = "_")
  colonnes <- c(list(unname(as.character(projet$noms_obs[obs]))), lapply(vars, function(v) {
    x <- projet$donnees[, as.character(v$uid)]
    if (length(v$categories)) factor(as.character(v$categories)[x], levels = as.character(v$categories)) else unname(as.numeric(x))
  }), lapply(ids, function(id) {
    x <- identifiants_observations(projet, match(id, as.character(projet$observation_id_names)), as.numeric(obs))
    x[!is.na(x) & !nzchar(trimws(x))] <- NA  # (un identifiant vide : manquant, comme regression.R le lit)
    x
  }))
  names(colonnes) <- noms
  list(donnees = as.data.frame(colonnes, check.names = FALSE, stringsAsFactors = FALSE),
       noms = stats::setNames(noms[1 + seq_along(vars)], vapply(vars, function(v) as.character(v$uid), "")),
       ids = stats::setNames(noms[1 + length(vars) + seq_along(ids)], ids),
       rangs = stats::setNames(seq_along(obs), obs))
}

# Les identifiants des observations dont les régressions de `definitions` font leurs groupes (effets
# aléatoires, temps de l'autocorrélation), et ceux que lit la condition d'un modèle (condition.R) : le
# script les ajoute à son tableau.
ids_script <- function(definitions, projet) {
  unique(unlist(lapply(definitions, function(d) {
    conditions <- ids_condition(projet, d)
    if (!est_regression(d)) return(conditions)
    specs <- c(specs_groupement(d$aleatoires), if (!is.null(d$correlation)) specs_groupement(d$correlation$temps),
               if (!is.null(d$correlation$dans)) specs_groupement(d$correlation$dans))
    c(vapply(Filter(function(s) identical(as.character(s$by)[1], "id"), specs), function(s) as.character(s$id)[1], ""), conditions)
  })))
}

# Les données dans le script : un data.frame, une colonne par ligne de code (ou plusieurs
# lignes pour une longue colonne), les nombres écrits sans perte.
code_donnees <- function(df, nom = "donnees") {
  colonnes <- vapply(seq_along(df), function(k) {
    x <- df[[k]]
    valeurs <- if (is.factor(x)) sprintf("factor(%s, levels = %s)", code_vecteur(as.character(x)), code_textes(levels(x))) else code_vecteur(x)
    sprintf("  %s = %s", deparse(as.name(names(df)[k]), backtick = TRUE), valeurs)
  }, "")
  c(sprintf("%s <- data.frame(", nom), paste0(colonnes, ","), "  check.names = FALSE, stringsAsFactors = FALSE)")
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

code_lire_csv <- function(df, fichier, langue = "fr", nom = "donnees") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr  # les messages que le script affiche
  classes <- vapply(df, function(x) if (is.numeric(x)) "numeric" else "character", "")
  facteurs <- which(vapply(df, is.factor, TRUE))
  fichier_nom <- if (identical(langue, "en")) "file" else "fichier"
  ici <- if (identical(langue, "en")) "here" else "ici"
  c(sprintf(tx("# Les données sont dans « %s », à côté de ce script.", "# The data are in “%s”, next to this script."), texte_sur(fichier)),
    sprintf("%s <- %s", fichier_nom, code_texte(fichier)),
    sprintf("if (!file.exists(%s)) {  # %s", fichier_nom, tx("pas dans le dossier de travail : dans celui du script", "not in the working folder: in the script's")),
    sprintf("  %s <- c(tryCatch(sys.frame(1)$ofile, error = function(e) NULL),", ici),
    sprintf("%ssub(\"^--file=\", \"\", grep(\"^--file=\", commandArgs(FALSE), value = TRUE)))", strrep(" ", nchar(ici) + 8)),
    sprintf("  if (length(%s)) %s <- file.path(dirname(%s[1]), %s)", ici, fichier_nom, ici, fichier_nom),
    "}",
    sprintf("if (!file.exists(%s)) stop(\"%s\", basename(%s), \"%s\")", fichier_nom, tx("Fichier de données introuvable : ", "Data file not found: "), fichier_nom,
            tx(". Il doit être à côté de ce script : ouvrir le script depuis son dossier, ou faire setwd() vers ce dossier.",
               ". It must be next to this script: open the script from its folder, or setwd() to that folder.")),
    sprintf("%s <- utils::read.csv(%s, check.names = FALSE, na.strings = \"\", fileEncoding = \"UTF-8\",", nom, fichier_nom),
    sprintf("%scolClasses = %s)", strrep(" ", nchar(nom) + 20), code_vecteur(unname(classes))),
    if (length(facteurs)) tx("# les variables qualitatives, avec leurs catégories dans l'ordre du projet", "# the qualitative variables, with their categories in the project's order"),
    vapply(facteurs, function(k) {
      col <- sprintf("%s[[%s]]", nom, code_texte(names(df)[k]))
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

# Des valeurs écrites c(a, b, ...), coupées en lignes d'environ 100 caractères, les suivantes
# alignées sous la première valeur ; `debut` et `fin` : ce qui les entoure sur la ligne.
code_valeurs <- function(valeurs, debut, fin = "", ouvrir = "c(") {
  if (length(valeurs) == 1 && nzchar(ouvrir)) return(paste0(debut, valeurs, fin))
  marge <- strrep(" ", nchar(debut) + nchar(ouvrir))
  lignes <- character(0)
  courante <- paste0(debut, ouvrir)
  for (i in seq_along(valeurs)) {
    s <- paste0(valeurs[i], if (i < length(valeurs)) "," else paste0(if (nzchar(ouvrir)) ")" else "", fin))
    if (i > 1 && nchar(courante) + 1 + nchar(s) > 98) {
      lignes <- c(lignes, courante)
      courante <- paste0(marge, s)
    } else courante <- paste0(courante, if (i > 1) " " else "", s)
  }
  c(lignes, courante)
}

# Des rangs, les suites (croissantes ou décroissantes) écrites a:b.
morceaux_rangs <- function(r) {
  r <- as.integer(r)
  if (!length(r)) return("integer(0)")
  morceaux <- character(0)
  i <- 1
  while (i <= length(r)) {
    j <- i
    pas <- if (i < length(r) && abs(r[i + 1] - r[i]) == 1) r[i + 1] - r[i] else 0
    while (pas != 0 && j < length(r) && r[j + 1] - r[j] == pas) j <- j + 1
    morceaux <- c(morceaux, if (j > i + 1 || (j == i + 1 && pas == 1)) sprintf("%d:%d", r[i], r[j]) else as.character(r[i:j]))
    i <- j + 1
  }
  morceaux
}

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

# Le code R qui crée la définition `d` d'un modèle, le projet s'appelant `p` dans ce code :
# modele_du_projet() pour un modèle importé d'un fichier .usp, definir_modele() sinon, avec les variables
# désignées par leur nom (pour le journal de la console).
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
      if (length(r$aleatoires)) c("# effets aléatoires emboîtés, du plus large au plus fin (voir moteur/R/regression.R)",
                                  sprintf("aleatoires <- %s", paste(deparse(r$aleatoires, width.cutoff = 500L), collapse = ""))),
      if (length(r$correlation)) sprintf("correlation <- %s", paste(deparse(r$correlation, width.cutoff = 500L), collapse = "")),
      if (length(r$blocs)) sprintf("blocs <- %s  # par numéro de variable", paste(deparse(unlist(r$blocs), width.cutoff = 500L), collapse = "")),
      if (length(r$modificateurs)) sprintf("modificateurs <- %s", paste(deparse(unlist(r$modificateurs), width.cutoff = 500L), collapse = "")),
      if (length(r$vc)) sprintf("vc <- %s  # groupes de la validation croisée", paste(deparse(r$vc, width.cutoff = 500L), collapse = "")),
      if (length(r$famille)) sprintf("famille <- %s  # loi de la réponse (regression.R)", q(r$famille)),
      if (length(r$lisses)) sprintf("lisses <- numeros(%s, %s)  # variables en courbe", p, vecteur(noms(r$lisses))),
      if (length(r$k_courbes)) sprintf("k_courbes <- %s  # dimension de base des courbes, par numéro de variable",
                                       paste(deparse(r$k_courbes, width.cutoff = 500L), collapse = "")),
      if (length(r$decalage)) sprintf("decalage <- numeros(%s, %s)  # l'effort, log en décalage", p, q(noms(r$decalage))),
      if (length(r$croises_reglages)) sprintf("croises_reglages <- %s  # échelle, bloc et modificateur des termes croisés",
                                              paste(deparse(r$croises_reglages, width.cutoff = 500L), collapse = "")),
      if (length(r$options)) sprintf("options <- %s  # les réglages propres de l'analyse (analyses.R)", paste(deparse(r$options, width.cutoff = 500L), collapse = "")),
      if (nzchar(r$condition)) sprintf("condition <- %s  # seulement les observations où elle est vraie (condition.R)", q(r$condition)),
      "",
      sprintf("d <- definir_modele(%s, %s, x = x, y = y, classe = classe, observations = observations,", p, q(r$type)),
      sprintf("                    echelle = echelles, transformations = transformations, croises = croises,%s",
              paste0(c(if (length(r$spectres)) " spectres = spectres,", if (length(r$test)) " test = test,",
                       if (length(r$aleatoires)) " aleatoires = aleatoires,", if (length(r$correlation)) " correlation = correlation,",
                       if (length(r$blocs)) " blocs = blocs,", if (length(r$modificateurs)) " modificateurs = modificateurs,",
                       if (length(r$vc)) " vc = vc,", if (length(r$famille)) " famille = famille,", if (length(r$lisses)) " lisses = lisses,",
                       if (length(r$decalage)) " decalage = decalage,", if (length(r$croises_reglages)) " croises_reglages = croises_reglages,",
                       if (length(r$k_courbes)) " k_courbes = k_courbes,", if (length(r$options)) " options = options,",
                       if (nzchar(r$condition)) " condition = condition,"), collapse = "")),
      sprintf("                    A = %s, G = %s, nom = %s, titre = %s)", q(r$A), q(r$G), q(r$nom), q(r$titre)))
  }
}
