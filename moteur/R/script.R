# Écrit un script R autonome qui refait un modèle de Nuée avec R seul, sans Nuée ni autre paquet :
# du code écrit pour ce modèle seulement, à lire de haut en bas. Il contient ses données, ses
# termes X et Y (transformations, prétraitement des spectres, mise à l'échelle), l'algorithme
# NIPALS de son type (ACP, PLS ou OPLS ; les valeurs manquantes n'y sont traitées que si les
# données en ont), sa validation croisée, puis ses résultats : R2 et Q2, scores et loadings, T2
# de Hotelling et DModX avec leurs limites ; pour une PLS (PLS-DA, OPLS) les VIP, les
# coefficients, la CV-ANOVA et le test de permutation (un appel en commentaire) ; le jeu de
# test ; des graphiques en R de base. D'après l'extension du chemin : un script (.R), ou un
# rapport R Markdown (.Rmd) ou Quarto (.qmd) qui y ajoute le texte des méthodes (methodes.R) et
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
  format <- c(r = "R", rmd = "Rmd", qmd = "qmd")[[tolower(tools::file_ext(chemin_script))]]
  tab <- tableau_script(projet)
  nom_projet <- as.character(projet$source %||% "projet")
  m <- m %||% ajuster(projet, d)
  pl <- plan_modele(projet, d, m, tab)
  b <- besoins_script(list(pl), seance = FALSE)
  fonctions <- code_fonctions(b, list(pl), langue)
  e <- environnement_script(tab, fonctions, langue)
  x <- code_un_modele(pl, m, b, e, langue)

  fichiers <- fichiers_script(chemin_script, donnees, rdata, renv, rproj, langue)
  code_donnees <- donnees_script(tab, fichiers, langue)
  lignes <- if (format == "R") {
    c(entete_script(pl, nom_projet, version, fichiers, langue), "",
      titre_partie(1, langue), fonctions,
      titre_partie(2, langue), code_donnees, "",
      titre_partie(3, langue), x$calcul, "",
      titre_partie(4, langue), x$sortie, "", texte_graphiques(langue), unlist(lapply(x$graphiques, function(g) c(g$code, ""))))
  } else {
    lignes_rapport(format, pl, langue, texte_methodes(projet, d, m, langue, version), fonctions, code_donnees, x)
  }
  ecrire_fichiers_script(lignes, fichiers, e, c(nom_script("donnees", langue), x$objets), if (format != "R") c("knitr", "rmarkdown"))
  list(script = chemin_script, chemins = unname(fichiers), ecart = x$ecart)
}

# Toute la séance dans un script .R : les données une fois, puis chaque modèle (`definitions`,
# et `modeles` ceux de l'application, dans le même ordre), ses résultats gardés dans une liste
# à son nom (M1, M2...). Mêmes options qu'ecrire_script() ; `ecart` nomme les modèles qui ne
# redonnent pas le leur.
ecrire_script_seance <- function(projet, definitions, modeles, chemin_script, dossier_moteur, donnees = c("script", "csv"),
                                 renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  donnees <- match.arg(donnees)
  langue <- if (identical(langue, "en")) "en" else "fr"
  tab <- tableau_script(projet)
  nom_projet <- as.character(projet$source %||% "projet")
  plans <- lapply(seq_along(definitions), function(k) plan_modele(projet, definitions[[k]], modeles[[k]], tab))
  b <- besoins_script(plans, seance = TRUE)
  fonctions <- code_fonctions(b, plans, langue)
  e <- environnement_script(tab, fonctions, langue)
  blocs <- list()
  resultats <- list()
  ecarts <- character(0)
  for (k in seq_along(plans)) {
    blocs[[k]] <- code_un_modele(plans[[k]], modeles[[k]], b, e, langue, seance = k)
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
    morceaux <- c(morceaux,
                  sprintf("# 3.%d %s : %s%s ----", k, noms[k], type_script(pl, langue), if (nzchar(pl$titre)) sprintf(" (%s)", texte_sur(pl$titre)) else ""),
                  x$calcul, "", x$sortie, if (pl$type != "acp") appel_permutations(pl, langue, noms[k]), "",
                  texte_graphiques(langue), unlist(lapply(x$graphiques, function(g) c(g$code, ""))),
                  commentaire(tx(sprintf("Les résultats de %s, gardés sous son nom (%s$composantes...).", noms[k], noms[k]),
                                 sprintf("The results of %s, kept under its name (%s$components...).", noms[k], noms[k]))),
                  code_valeurs(sprintf("%s = %s", x$objets, x$objets), sprintf("%s <- list(", noms[k]), ")", ouvrir = ""), "")
  }
  fichiers <- fichiers_script(chemin_script, donnees, rdata, renv, rproj, langue)
  lignes <- c(entete_seance(plans, noms, nom_projet, version, fichiers, langue), "",
              titre_partie(1, langue), fonctions,
              titre_partie(2, langue), donnees_script(tab, fichiers, langue), "",
              sprintf("# 3. %s ----", tx("Modèles et résultats", "Models and results")), "", morceaux)
  ecrire_fichiers_script(lignes, fichiers, e, c(nom_script("donnees", langue), noms), character(0))
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
evaluer_script <- function(lignes, e) {
  probleme <- NULL
  tryCatch(withCallingHandlers(eval(parse(text = lignes, encoding = "UTF-8"), envir = e),
                               warning = function(w) {
                                 probleme <<- sprintf(traduire("avertissement : %s", "warning: %s"), conditionMessage(w))
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
       vc_par = m$vc_par %||% "", vc_fichier = identical(as.character(d[["cv"]]$by), "groups"),
       rangs = unname(tab$rangs[as.character(m$observations)]), rangs_test = unname(tab$rangs[as.character(test)]),
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
  list(acp = any(types == "acp"), pls = any(types == "pls"), opls = any(types == "opls"),
       opls_general = any(vapply(plans, function(pl) pl$type == "opls" && pl$Ap > 1, TRUE)),
       manquantes = any(vapply(plans, `[[`, TRUE, "manquantes")), y_manquantes = any(vapply(plans, `[[`, TRUE, "y_manquantes")),
       vides = any(vapply(plans, `[[`, TRUE, "vides")),
       plsda = any(vapply(plans, `[[`, TRUE, "plsda")), spectres = etapes, seance = seance)
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

# Les observations du modèle (et du jeu de test) : le tableau `d`.
code_observations <- function(pl, tx) {
  n <- length(pl$rangs)
  nt <- length(pl$rangs_test)
  total <- length(pl$noms_tableau)
  if (!nt && identical(as.integer(pl$rangs), seq_len(total))) {
    return(c(commentaire(tx(sprintf("Les %d observations du tableau.", n), sprintf("The %d observations of the table.", n))), "d <- donnees"))
  }
  exclues <- setdiff(seq_len(total), c(pl$rangs, pl$rangs_test))
  quelles <- if (length(exclues) && length(exclues) <= 8) liste_textes(texte_sur(pl$noms_tableau[exclues]), tx("et", "and"))
  sauf <- if (length(exclues) && !is.unsorted(pl$rangs, strictly = TRUE)) {
    if (!is.null(quelles)) tx(sprintf(" : toutes sauf %s", quelles), sprintf(": all but %s", quelles))
    else tx(sprintf(" : %d exclues", length(exclues)), sprintf(": %d excluded", length(exclues)))
  } else if (is.unsorted(pl$rangs, strictly = TRUE)) tx(", dans l'ordre du modèle", ", in the model's order") else ""
  if (!nt) {
    return(c(commentaire(tx(sprintf("Les %d observations du modèle (leurs rangs dans le tableau)%s.", n, sauf),
                            sprintf("The %d observations of the model (their rows in the table)%s.", n, sauf))),
             code_valeurs(morceaux_rangs(pl$rangs), "lignes <- "), "d <- donnees[lignes, ]"))
  }
  c(commentaire(tx(sprintf("Les %d observations du modèle%s, puis les %d du jeu de test : laissées hors du modèle (ni l'ajustement ni la validation croisée ne les voient), puis prédites par lui. Leurs rangs dans le tableau.", n, sauf, nt),
                   sprintf("The %d observations of the model%s, then the %d of the test set: left out of the model (neither the fit nor the cross-validation sees them), then predicted by it. Their rows in the table.", n, sauf, nt))),
    code_valeurs(morceaux_rangs(pl$rangs), "lignes <- "),
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
    if (na || b$acp) modele_projeter(tx),  # (la validation croisée d'une ACP ignore des termes comme des valeurs manquantes)
    if (b$acp) c(modele_composante_acp(tx, na), "", modele_acp(tx, na, b$vides), "", modele_vc_acp(tx, na)),
    if (b$pls || b$opls) modele_composante_pls(tx, na),
    if (b$pls) c(modele_pls(tx, na, b$vides), "", modele_vc_pls(tx, na, b$y_manquantes)),
    if (b$opls) c(if (general) c(modele_composantes_pls(tx, na), "", modele_base_poids_y(tx, na), ""),
                  modele_opls_noyau(tx, na, general), "", modele_opls(tx, na, b$vides, general), "",
                  modele_opls_predire(tx, na, general), "", modele_vc_opls(tx, na, general)),
    modele_echelle(tx),
    if (b$plsda) modele_la_plus_grande(tx),
    if (b$pls || b$opls) modele_permutations(tx, seul, na),
    if ("snv" %in% b$spectres) modele_snv(tx),
    if ("msc" %in% b$spectres) modele_msc(tx),
    if ("sg" %in% b$spectres) modele_savitzky_golay(tx),
    if ("detrend" %in% b$spectres) modele_detrend(tx, na),
    if ("ligne_base" %in% b$spectres) modele_ligne_base(tx))
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

modele_echelle <- function(tx) c(
  commentaire(tx("Chaque colonne de X centrée puis multipliée par son poids : (x - centre) * poids ; et des noms de colonnes numérotés (t1, t2...).",
                 "Each column of X centred then multiplied by its weight: (x - centre) * weight; and numbered column names (t1, t2...).")),
  "mettre_a_echelle <- function(X, centre, poids) sweep(sweep(X, 2, centre), 2, poids, \"*\")",
  "nommer <- function(M, prefixe) {",
  "  colnames(M) <- paste0(prefixe, seq_len(ncol(M)))",
  "  M",
  "}")

modele_la_plus_grande <- function(tx) c(
  commentaire(tx("La classe de chaque ligne de Y : celle de sa plus grande valeur.", "The class of each row of Y: that of its largest value.")),
  "la_plus_grande <- function(Y) colnames(Y)[apply(Y, 1, function(v) which.max(v)[1])]")

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
  donnees = "data", lignes = "rows", lignes_test = "test_rows", modele = "in_model", y_observe = "y_observed",
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
  groupes_vc = "cv_groups")

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
    if (n && (w %in% c(";", ":", "?", "!", "»") || unites[n] == "«")) unites[n] <- paste(unites[n], w) else unites <- c(unites, w)
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

entete_seance <- function(plans, noms, nom_projet, version, fichiers, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  modele <- function(k) {
    pl <- plans[[k]]
    sprintf(tx("#   %s%s : %s, %d composante%s, %d observations, %d termes X", "#   %s%s: %s, %d component%s, %d observations, %d X terms"),
            noms[k], if (nzchar(pl$titre)) sprintf(" (%s)", texte_sur(pl$titre)) else "", type_script(pl, langue), pl$A, if (pl$A > 1) "s" else "", pl$N, pl$K)
  }
  c(sprintf(tx("# Les %d modèles de la séance du projet « %s » :", "# The %d models of the session of the project “%s”:"), length(noms), texte_sur(nom_projet)),
    vapply(seq_along(noms), modele, ""),
    sprintf(tx("# Script R écrit par Nuée %s le %s.", "# R script written by Nuée %s on %s."), version, format(Sys.Date())),
    tx("# Il refait ces modèles avec R seul, sans Nuée ni autre paquet : le calcul est écrit pour eux, de",
       "# It redoes these models with R alone, without Nuée or any package: the computation is written for"),
    tx("# leurs données à leurs résultats, qui sont ceux de Nuée. Ses parties :", "# them, from their data to their results, which are those of Nuée. Its parts:"),
    tx("#   1. Fonctions : l'algorithme NIPALS, la validation croisée", "#   1. Functions: the NIPALS algorithm, the cross-validation"),
    tx("#   2. Données", "#   2. Data"),
    tx("#   3. Modèles et résultats : un bloc par modèle, ses résultats gardés dans une liste à son nom",
       "#   3. Models and results: one block per model, its results kept in a list under its name"),
    tx("# Dans RStudio, Ctrl+Maj+O affiche ces parties ; Ctrl+Maj+S lance tout le script.", "# In RStudio, Ctrl+Shift+O shows these parts; Ctrl+Shift+S runs the whole script."),
    sprintf(tx("# Écrit avec R %s ; il n'utilise que R (stats et graphics sont livrés avec R).", "# Written with R %s; it uses R alone (stats and graphics come with R)."),
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
    bloc("composantes", kable("composantes", 3)),
    unlist(lapply(x$graphiques, function(g) bloc(sprintf("%s, fig.width = %s, fig.height = %s", g$nom, g$largeur, g$hauteur), g$code))),
    bloc("limites", kable("t(limites)", 3)),
    if (pl$cv_anova) bloc("cv-anova", kable("cv_anova[[1]]", 4)),
    if (pl$plsda) bloc("classification", kable("classification", NA)),
    if (test && pls && !pl$plsda) bloc("test", kable("test", 3)),
    if (test && pl$plsda) bloc("test", kable("classification_test", NA)),
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
# pour les variables qualitatives, avec toutes leurs catégories, dans leur ordre). Les noms
# des variables sont rendus uniques ; `noms` les donne par numéro de variable du projet, `rangs`
# le rang de chaque observation (sa ligne dans le tableau).
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
      "",
      sprintf("d <- definir_modele(%s, %s, x = x, y = y, classe = classe, observations = observations,", p, q(r$type)),
      sprintf("                    echelle = echelles, transformations = transformations, croises = croises,%s%s",
              if (length(r$spectres)) " spectres = spectres," else "", if (length(r$test)) " test = test," else ""),
      sprintf("                    A = %s, G = %s, nom = %s, titre = %s)", q(r$A), q(r$G), q(r$nom), q(r$titre)))
  }
}
