# Arbres de décision (rpart : Breiman et al., 1984 ; Therneau et Atkinson) : un arbre de
# régression (une réponse quantitative) ou de classement (une variable de classe), construit sur
# les valeurs brutes des variables X (transformées s'il y a lieu) pour que ses seuils se lisent
# dans leurs unités ; les valeurs manquantes des X sont prises en charge par des variables de
# substitution. Comme le script d'arbres de régression de référence :
#   - un arbre volontairement grand (au moins `minbucket` observations par feuille, mais au plus
#     le sixième des observations, au plus `maxdepth` niveaux, cp = 0,001, `maxsurrogate`
#     variables de substitution), puis élagué par
#     une validation croisée INTERNE par les groupes de la définition (un site, par exemple) : le
#     plus petit arbre dont l'erreur de validation croisée ne dépasse pas l'erreur minimale de
#     plus d'une erreur-type (règle « 1-ET »), ou celui de l'erreur minimale ;
#   - une validation croisée EXTERNE : chaque groupe prédit par un arbre refait entièrement sans
#     lui (sa validation croisée interne et son élagage compris) ; Q² = 1 - PRESS / SS ;
#   - les règles de chaque feuille, la première coupure, l'importance des variables, et à la
#     demande la stabilité de la première coupure sur des tirages bootstrap des groupes.
# Les groupes de la validation croisée interne sont donnés à rpart DANS son contrôle
# (rpart.control(xval = groupes)) : donnés à côté d'un argument `control`, ils seraient remplacés
# par les 10 groupes tirés au hasard de ce contrôle.
#
# En tête, les outils communs aux analyses de ce fichier, de discrimination.R (LDA) et de
# kmoyennes.R (k-moyennes) ; leur export vers R est dans classement_script.R.

# ---------------------------------------------------------------- outils communs

# Évalue `expr` après set.seed(graine) avec le générateur de R par défaut (comme le script
# exporté, même si la console R en a choisi un autre), puis rend au générateur son genre et son
# état : un calcul reproductible qui ne change pas le hasard du reste de la séance.
avec_graine <- function(graine, expr) {
  avant <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv())
  genre <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(genre[1], genre[2], genre[3]))  # (le genre de la séance rendu, sans redire ses avertissements)
    if (is.null(avant)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", avant, envir = globalenv())
  })
  set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  expr
}

# Un réglage numérique de d$options, borné (arrondi si `entier`), ou sa valeur par défaut.
option_nombre <- function(d, nom, defaut, min = -Inf, max = Inf, entier = FALSE) {
  v <- suppressWarnings(as.numeric(unlist(d$options[[nom]])))
  v <- if (length(v) && is.finite(v[1])) v[1] else defaut
  if (entier) v <- round(v)
  min(max, max(min, v))
}

# Un réglage de d$options parmi `choix` (le premier par défaut).
option_choix <- function(d, nom, choix) {
  v <- as.character(unlist(d$options[[nom]]))
  if (length(v) && !is.na(v[1]) && v[1] %in% choix) v[1] else choix[1]
}

# La classe de chaque observation : un facteur des catégories de la variable de classe (NA :
# inconnue).
classe_observations <- function(projet, classe, obs) {
  v <- variable_du_projet(projet, classe)
  if (!length(v$categories)) {
    stop(sprintf(traduire("La variable de classe « %s » doit être qualitative.", "The class variable “%s” must be qualitative."), v$ids[1]), call. = FALSE)
  }
  x <- colonne(projet, classe, obs)
  factor(as.character(v$categories)[x], levels = as.character(v$categories))
}

# Le groupe de validation croisée de ces observations (parmi celles de la définition).
groupes_observations <- function(projet, d, obs) groupes_definition(projet, d)[match(obs, d$observations)]

# Une indicatrice 0/1 par classe (classification_plsda : la classe observée).
indicatrices <- function(classe, classes) {
  matrix(vapply(classes, function(k) as.numeric(as.character(classe) == k), numeric(length(classe))),
         ncol = length(classes), dimnames = list(NULL, classes))
}

# La classe de plus grande probabilité de chaque ligne (NA sans probabilité).
classe_la_plus_probable <- function(P, classes) {
  if (!nrow(P)) return(character(0))
  apply(P, 1, function(v) if (all(is.na(v))) NA_character_ else classes[which.max(v)])
}

# Le tableau de classement (classes observées en lignes, prédites en colonnes) des observations
# dont les deux sont connues.
table_classement <- function(observee, predite, classes) {
  ok <- !is.na(observee) & !is.na(predite)
  unname(unclass(table(factor(as.character(observee)[ok], classes), factor(as.character(predite)[ok], classes))))
}

# Le jeu de test d'un classement (jeu_test.R) : la part bien classée, les classes, le tableau,
# sur les observations d'une classe que le modèle connaît (il ne peut prédire une classe qu'il
# n'a pas vue : `inconnues` les compte).
classement_test <- function(observee, predite, classes) {
  observee <- as.character(observee)
  predite <- as.character(predite)
  ok <- !is.na(observee) & !is.na(predite) & observee %in% classes
  list(bien_classees = if (any(ok)) mean(observee[ok] == predite[ok]) else NA_real_, classes = classes,
       table = table_classement(observee, predite, classes), inconnues = sum(!is.na(observee) & !observee %in% classes))
}

# Des noms de colonnes uniques pour un data.frame de R (`reserves` : des noms déjà pris), sans
# accent grave (il fermerait le nom dans une formule).
noms_colonnes <- function(noms, reserves = character(0)) {
  noms <- gsub("`", "'", as.character(noms))
  noms[!nzchar(noms)] <- paste0("V", which(!nzchar(noms)))
  make.unique(c(reserves, noms), sep = "_")[length(reserves) + seq_along(noms)]
}

REFERENCES_CLASSEMENT <- list(
  breiman = "Breiman, L., Friedman, J. H., Olshen, R. A., & Stone, C. J. (1984). Classification and regression trees. Wadsworth.",
  rpart = "Therneau, T., & Atkinson, B. (%s). rpart: Recursive partitioning and regression trees (R package version %s). https://CRAN.R-project.org/package=rpart",
  fisher = "Fisher, R. A. (1936). The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7(2), 179–188. https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  venables = "Venables, W. N., & Ripley, B. D. (2002). Modern applied statistics with S (4th ed.). Springer. https://doi.org/10.1007/978-0-387-21706-2",
  hartigan = "Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society: Series C (Applied Statistics), 28(1), 100–108. https://doi.org/10.2307/2346830",
  rousseeuw = "Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. Journal of Computational and Applied Mathematics, 20, 53–65. https://doi.org/10.1016/0377-0427(87)90125-7",
  cluster = "Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., & Hornik, K. (%s). cluster: Cluster analysis basics and extensions (R package version %s). https://CRAN.R-project.org/package=cluster")

# La phrase des logiciels d'un texte des méthodes de ces analyses (R, Nuée et le paquet : rpart,
# MASS, cluster ; NULL : R seul) et ses références (`cites`, avec celles de R et de Nuée), dans
# l'ordre alphabétique.
references_classement <- function(cites, paquet, fr, version) {
  annee_r <- R.version$year
  annee_nuee <- format(Sys.Date(), "%Y")
  desc <- if (!is.null(paquet)) utils::packageDescription(paquet)
  annee_paquet <- if (identical(paquet, "MASS")) "2002" else if (!is.null(paquet)) substr(desc$Date %||% desc$Packaged %||% annee_r, 1, 4)
  auteurs <- if (!is.null(paquet)) switch(paquet, rpart = "Therneau & Atkinson", MASS = "Venables & Ripley", cluster = "Maechler et al.")
  phrase <- sprintf(if (fr) "Les calculs ont été faits avec Nuée%s (4rtefakt, %s) et R %s (R Core Team, %s)%s."
                    else "Computations were done with Nuée%s (4rtefakt, %s) and R %s (R Core Team, %s)%s.",
                    if (nzchar(version)) paste0(" ", version) else "", annee_nuee, paste(R.version$major, R.version$minor, sep = "."), annee_r,
                    if (is.null(paquet)) "" else sprintf(if (fr) ", paquet %s %s (%s, %s)" else ", package %s %s (%s, %s)", paquet, desc$Version, auteurs, annee_paquet))
  cites <- union(cites, c("r", "nuee"))
  refs <- vapply(cites, function(k) switch(k,
    r = sprintf(REFERENCES$r, annee_r),
    nuee = sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
    rpart = sprintf(REFERENCES_CLASSEMENT$rpart, annee_paquet, desc$Version),
    cluster = sprintf(REFERENCES_CLASSEMENT$cluster, annee_paquet, desc$Version),
    REFERENCES_CLASSEMENT[[k]]), "")
  list(phrase = phrase, references = unname(sort(refs)))
}

# Des nombres et des mots pour un texte des méthodes (virgule décimale en français).
nombre_texte <- function(x, chiffres = 2, fr = TRUE) formatC(x, format = "f", digits = chiffres, decimal.mark = if (fr) "," else ".")
pourcent_texte <- function(x, fr = TRUE) paste0(formatC(100 * x, format = "f", digits = 1, decimal.mark = if (fr) "," else "."), if (fr) " %" else "%")
liste_mots <- function(x, fr) if (length(x) <= 1) paste(x) else paste(paste(x[-length(x)], collapse = ", "), if (fr) "et" else "and", x[length(x)])
guillemets_texte <- function(x, fr) if (fr) sprintf("« %s »", x) else sprintf("“%s”", x)

# Les groupes de la validation croisée, dans un texte des méthodes : « par « site » (12 groupes) ».
groupes_texte <- function(vc_par, G, fr) {
  if (nzchar(vc_par)) sprintf(if (fr) "par %s (%d groupes)" else "by %s (%d groups)", guillemets_texte(vc_par, fr), G)
  else sprintf(if (fr) "en %d groupes" else "in %d groups", G)
}

# Un pourcentage pour la ligne d'un modèle dans l'explorateur.
fmt_pourcent <- function(x) if (is.na(x)) "—" else sprintf("%.0f %%", 100 * x)

# ---------------------------------------------------------------- les arbres

# Les réglages d'un arbre (d$options) ; par défaut, ceux du script de référence.
reglages_arbre <- function(d) list(
  minbucket = option_nombre(d, "minbucket", 20, 1, 1e6, TRUE), cp = option_nombre(d, "cp", 0.001, 0, 1),
  maxdepth = option_nombre(d, "maxdepth", 5, 1, 30, TRUE), maxsurrogate = option_nombre(d, "maxsurrogate", 5, 0, 20, TRUE),
  elagage = option_choix(d, "elagage", c("1se", "min")), tirages = option_nombre(d, "tirages", 200, 10, 2000, TRUE))

# Le minimum d'observations par feuille : celui demandé, mais au plus le sixième des `n`
# observations. rpart ne coupe un nœud que s'il en compte trois fois plus (minsplit) : ainsi la
# racine et ses deux enfants peuvent se couper, où 20 par feuille sur 50 observations ne
# laisseraient aucune coupure ; l'élagage par validation croisée choisit ensuite la taille. À
# partir de six fois le nombre demandé (120 observations pour 20), il reste tel quel.
minimum_feuille <- function(demande, n) as.integer(min(demande, max(1, n %/% 6)))

# Les variables X d'un arbre : numéro, nom, colonne (son nom dans le tableau de rpart, unique),
# catégories (une qualitative), transformation et bloc.
variables_x_brutes <- function(projet, d) {
  simples <- Filter(function(t) length(t$variables) == 1, d$x)
  uids <- unique(vapply(simples, function(t) as.numeric(t$variables), 0))
  noms <- vapply(uids, function(u) as.character(variable_du_projet(projet, u)$ids[1]), "")
  colonnes <- noms_colonnes(noms, ".y")
  lapply(seq_along(uids), function(j) {
    v <- variable_du_projet(projet, uids[j])
    t <- Find(function(t) as.numeric(t$variables) == uids[j], simples)
    b <- t$scaling$block
    list(uid = uids[j], nom = noms[j], colonne = colonnes[j], categories = if (length(v$categories)) as.character(v$categories),
         transform = if (!length(v$categories)) t$transform,
         bloc = if (length(b) && !is.na(b[1]) && nzchar(as.character(b[1]))) as.character(b[1]) else "")
  })
}

# Les X bruts de ces observations, une colonne par variable (un facteur de toutes ses catégories
# pour une qualitative) ; `projet` peut n'avoir que des données (la prédiction d'un fichier).
x_bruts <- function(projet, vars, obs) {
  colonnes <- lapply(vars, function(v) {
    x <- colonne(projet, v$uid, obs)
    if (length(v$categories)) factor(v$categories[x], levels = v$categories)
    else if (!is.null(v$transform)) suppressWarnings(transformer(as.numeric(x), v$transform)) else as.numeric(x)
  })
  names(colonnes) <- vapply(vars, `[[`, "", "colonne")
  as.data.frame(colonnes, check.names = FALSE, stringsAsFactors = FALSE)
}

# Un arbre grand, puis élagué par sa validation croisée interne (`plis` : le groupe de chaque
# ligne), par la règle 1-ET ou à l'erreur minimale. Un arbre de classement n'a que les classes
# présentes dans ses lignes (rpart ne prédit pas une classe qu'il n'a pas vue).
arbre_elague <- function(dat, plis, methode, ctl, regle) {
  if (methode == "class") dat$.y <- droplevels(dat$.y)
  ctl$xval <- as.integer(factor(plis))
  complet <- rpart::rpart(.y ~ ., data = dat, method = methode, control = ctl, model = FALSE, x = FALSE, y = TRUE)
  ct <- complet$cptable
  i <- which.min(ct[, "xerror"])
  cp <- if (identical(regle, "min")) ct[i, "CP"] else ct[which(ct[, "xerror"] <= ct[i, "xerror"] + ct[i, "xstd"])[1], "CP"]
  list(complet = complet, arbre = rpart::prune(complet, cp = cp), cp = unname(cp))
}

# La probabilité de chaque classe (0 pour une classe que l'arbre n'a pas vue).
proba_arbre <- function(a, donnees, classes) {
  P <- matrix(0, nrow(donnees), length(classes), dimnames = list(NULL, classes))
  if (!nrow(donnees)) return(P)
  p <- stats::predict(a, donnees, type = "prob")
  k <- intersect(colnames(p), classes)
  P[, k] <- p[, k, drop = FALSE]
  P
}

# La validation croisée externe : chaque groupe prédit par un arbre refait sans lui, sa
# validation croisée interne (par les autres groupes) et son élagage compris. Une réponse
# quantitative : la prédiction de chaque ligne ; une classe : la probabilité de chaque classe.
validation_arbre <- function(dat, groupes, methode, ctl, regle) {
  classes <- if (methode == "class") levels(dat$.y)
  pred <- if (is.null(classes)) rep(NA_real_, nrow(dat)) else matrix(NA_real_, nrow(dat), length(classes), dimnames = list(NULL, classes))
  for (g in unique(groupes)) {
    k <- groupes == g
    restantes <- if (!is.null(classes)) unique(as.character(dat$.y[!k]))
    if (length(restantes) == 1) {  # (sans ce groupe, une seule classe : tout arbre la prédit ; rpart ne s'ajuste pas sur une classe)
      pred[k, ] <- 0
      pred[k, restantes] <- 1
      next
    }
    a <- arbre_elague(dat[!k, , drop = FALSE], groupes[!k], methode, ctl, regle)$arbre
    if (is.null(classes)) pred[k] <- stats::predict(a, dat[k, , drop = FALSE])
    else pred[k, ] <- proba_arbre(a, dat[k, , drop = FALSE], classes)
  }
  pred
}

# Les observations d'un jeu de test dont l'arbre ou la LDA n'a pas vu la classe, laissées hors de
# la part bien classée (« , sans les 3 d'une classe inconnue du modèle »).
classes_inconnues_texte <- function(n, fr) {
  if (!isTRUE(n > 0)) return("")
  sprintf(if (fr) ", sans %s d'une classe que le modèle n'a pas vue" else ", without the %s of a class the model hasn't seen",
          if (n == 1) (if (fr) "celle" else "one") else sprintf(if (fr) "les %d" else "%d", n))
}

# Le nom de la variable d'une colonne de rpart.
nom_variable_arbre <- function(colonne, vars) {
  noms <- vapply(vars, `[[`, "", "nom")
  noms[match(colonne, vapply(vars, `[[`, "", "colonne"))]
}

# Les étiquettes de rpart : la coupure qui mène à chaque nœud (« h_gel>=12.5 », « variete=a,b »),
# avec les chiffres de path.rpart.
etiquettes_arbre <- function(a) labels(a, digits = 4, minlength = 0L)

# La première coupure (celle de la racine) : variable, seuil (pour une variable quantitative)
# et règle de sa branche gauche ; NA pour un arbre sans coupure.
premiere_coupure <- function(a, vars) {
  if (nrow(a$frame) == 1) return(list(variable = NA_character_, seuil = NA_real_, regle = NA_character_))
  s <- a$splits[1, ]
  list(variable = nom_variable_arbre(as.character(a$frame$var[1]), vars),
       seuil = if (abs(s[["ncat"]]) == 1) unname(s[["index"]]) else NA_real_, regle = etiquettes_arbre(a)[2])
}

# La part de la classe majoritaire de chaque nœud d'un arbre de classement.
part_majoritaire <- function(a, i) {
  K <- length(attr(a, "ylevels"))
  vapply(i, function(k) max(a$frame$yval2[k, 1 + K + seq_len(K)]), 0)
}

# Les feuilles d'un arbre : leur numéro, nœud, effectif, valeur (moyenne, ou classe et sa part)
# et leur règle (le chemin depuis la racine, comme path.rpart).
regles_arbre <- function(a, classes = NULL) {
  f <- a$frame
  feuilles <- which(f$var == "<leaf>")
  noeuds <- as.numeric(rownames(f)[feuilles])
  regle <- if (nrow(f) == 1) traduire("(aucune coupure)", "(no split)") else {
    vapply(rpart::path.rpart(a, nodes = noeuds, print.it = FALSE), function(x) paste(x[-1], collapse = " & "), "")
  }
  out <- data.frame(feuille = seq_along(feuilles), noeud = noeuds, n = f$n[feuilles], stringsAsFactors = FALSE)
  if (is.null(classes)) out$valeur <- f$yval[feuilles]
  else {
    out$valeur <- attr(a, "ylevels")[f$yval[feuilles]]
    out$part <- part_majoritaire(a, feuilles)
  }
  out$regle <- unname(regle)
  out
}

# Les nœuds d'un arbre, pour le dessiner : numéro (ses enfants : 2n et 2n + 1), feuille ou non,
# variable de sa coupure, effectif, la coupure qui y mène, valeur (moyenne, ou classe et sa part).
noeuds_arbre <- function(a, vars, classes = NULL) {
  f <- a$frame
  feuille <- f$var == "<leaf>"
  branche <- etiquettes_arbre(a)
  branche[1] <- ""
  out <- data.frame(id = as.numeric(rownames(f)), feuille = feuille,
                    variable = ifelse(feuille, NA_character_, nom_variable_arbre(as.character(f$var), vars)),
                    n = f$n, branche = unname(branche), stringsAsFactors = FALSE)
  if (is.null(classes)) out$valeur <- f$yval
  else {
    out$valeur <- attr(a, "ylevels")[f$yval]
    out$part <- part_majoritaire(a, seq_len(nrow(f)))
  }
  out
}

# L'importance des variables (rpart : coupures et substitutions), en % de leur somme.
importance_arbre <- function(a, vars) {
  vi <- a$variable.importance
  if (!length(vi)) return(data.frame(variable = character(0), bloc = character(0), importance = numeric(0), pourcentage = numeric(0)))
  j <- match(names(vi), vapply(vars, `[[`, "", "colonne"))
  data.frame(variable = vapply(vars[j], `[[`, "", "nom"), bloc = vapply(vars[j], `[[`, "", "bloc"), importance = unname(vi),
             pourcentage = 100 * unname(vi) / sum(vi), stringsAsFactors = FALSE)
}

# La table d'élagage de l'arbre complet : cp, coupures, feuilles, erreur relative et erreur de
# validation croisée (relative, et son erreur-type).
elagage_arbre <- function(m) {
  ct <- m$complet$cptable
  data.frame(cp = ct[, "CP"], coupures = ct[, "nsplit"], feuilles = ct[, "nsplit"] + 1, erreur_relative = ct[, "rel error"],
             xerror = ct[, "xerror"], xstd = ct[, "xstd"], row.names = NULL)
}

# La réponse d'un arbre : une classe (d$classe, arbre de classement) ou une variable Y
# quantitative (arbre de régression), jamais les deux ; TRUE pour un arbre de classement.
verifier_arbre <- function(projet, d) {
  classement <- length(d$classe) > 0 && !is.na(d$classe[1])
  if (classement && length(d$y)) {
    stop(traduire("Un arbre a une seule réponse : une variable Y (arbre de régression) ou une classe (arbre de classement), pas les deux.",
                  "A tree has a single response: a Y variable (regression tree) or a class (classification tree), not both."), call. = FALSE)
  }
  ys <- unique(vapply(d$y, function(t) as.numeric(t$variables[1]), 0))  # (une variable qualitative : un terme par catégorie)
  if (!classement && length(ys) == 1 && length(variable_du_projet(projet, ys)$categories)) {
    stop(traduire("La réponse d'un arbre de régression est quantitative ; une variable qualitative est la classe d'un arbre de classement (rôle Classe).",
                  "A regression tree's response is quantitative; a qualitative variable is the class of a classification tree (Class role)."), call. = FALSE)
  }
  if (!classement && length(d$y) != 1) {
    stop(traduire("Un arbre demande une réponse : une variable Y quantitative (arbre de régression) ou une variable de classe (arbre de classement).",
                  "A tree needs a response: a quantitative Y variable (regression tree) or a class variable (classification tree)."), call. = FALSE)
  }
  if (any(lengths(lapply(d$x, `[[`, "variables")) > 1)) {
    stop(traduire("Un arbre fait lui-même ses interactions : pas de termes croisés.", "A tree makes its own interactions: no cross terms."), call. = FALSE)
  }
  if (!length(d$x)) stop(traduire("Un arbre demande au moins une variable X.", "A tree needs at least one X variable."), call. = FALSE)
  classement
}

ajuster_tree <- function(projet, d) {
  r <- reglages_arbre(d)
  classement <- verifier_arbre(projet, d)
  vars <- variables_x_brutes(projet, d)
  obs <- d$observations
  X <- x_bruts(projet, vars, obs)
  if (classement) {
    y <- classe_observations(projet, d$classe, obs)
    nom_y <- as.character(variable_du_projet(projet, d$classe)$ids[1])
  } else {
    ty <- d$y[[1]]
    y <- suppressWarnings(transformer(valeurs_simples(projet, ty, obs), ty$transform))
    nom_y <- as.character(ty$name)
  }
  # (les lignes que rpart garde : une réponse et au moins une valeur X, comme na.rpart)
  connu <- !is.na(y) & rowSums(!is.na(X)) > 0
  if (sum(connu) < 4) stop(traduire("Trop peu d'observations avec une réponse et au moins une valeur X.", "Too few observations with a response and at least one X value."), call. = FALSE)
  garde <- obs[connu]
  groupes <- groupes_observations(projet, d, garde)
  if (length(unique(groupes)) < 3) {
    stop(traduire("Un arbre demande au moins trois groupes de validation croisée : son élagage est fait par une validation croisée interne, refaite sans chaque groupe.",
                  "A tree needs at least three cross-validation groups: it is pruned by an internal cross-validation, redone without each group."), call. = FALSE)
  }
  dat <- data.frame(.y = if (classement) droplevels(y[connu]) else y[connu], X[connu, , drop = FALSE], check.names = FALSE)
  if (classement && nlevels(dat$.y) < 2) stop(traduire("Un arbre de classement demande au moins deux classes.", "A classification tree needs at least two classes."), call. = FALSE)
  ctl <- rpart::rpart.control(minbucket = minimum_feuille(r$minbucket, nrow(dat)), cp = r$cp, maxdepth = r$maxdepth, maxsurrogate = r$maxsurrogate)
  methode <- if (classement) "class" else "anova"
  e <- arbre_elague(dat, groupes, methode, ctl, r$elagage)
  vc <- validation_arbre(dat, groupes, methode, ctl, r$elagage)
  m <- list(type = "TREE", classement = classement, N = nrow(dat), K = length(vars), observations = garde, exclues = setdiff(obs, garde),
            y = list(nom = nom_y, transform = if (!classement) d$y[[1]]$transform), variables = vars, reglages_arbre = r, donnees = dat,
            groupes = groupes, G = length(unique(groupes)), vc_par = nom_groupes_vc(projet, d), complet = e$complet, ajustement = e$arbre,
            cp = e$cp, controle = ctl, classes = if (classement) levels(dat$.y), manquantes_x = sum(rowSums(is.na(dat[-1])) > 0))
  if (classement) {
    m$proba <- proba_arbre(e$arbre, dat, m$classes)
    m$classification <- classification_plsda(indicatrices(dat$.y, m$classes), m$proba, vc, m$classes)
    m$vc <- list(proba = vc)
  } else {
    m$ajuste <- as.vector(stats::predict(e$arbre))
    ss <- sum((dat$.y - mean(dat$.y))^2)
    m$r2 <- 1 - sum((dat$.y - m$ajuste)^2) / ss
    m$vc <- list(prediction = vc, Q2 = 1 - sum((dat$.y - vc)^2) / ss, RMSEcv = sqrt(mean((dat$.y - vc)^2)))
  }
  m
}

# Le jeu de test d'un arbre : ses observations prédites (comme jeu_test.R les décrit) ;
# ajuster_analyse (analyses.R) l'appelle après l'ajustement.
tester_tree <- function(projet, m, d) {
  test <- as.numeric(d$test)
  X <- x_bruts(projet, m$variables, test)
  noms <- unname(projet$noms_obs[as.character(test)])
  vides <- rowSums(!is.na(X)) == 0
  if (m$classement) {
    y <- as.character(classe_observations(projet, d$classe, test))
    P <- proba_arbre(m$ajustement, X, m$classes)
    P[vides, ] <- NA
    p <- classe_la_plus_probable(P, m$classes)
    obs <- data.frame(uid = test, nom = noms, hors_domaine = FALSE, classe = y, classe_predite = p, stringsAsFactors = FALSE)
    return(list(n = length(test), hors_domaine = 0, observations = obs, classement = classement_test(y, p, m$classes)))
  }
  y <- suppressWarnings(transformer(valeurs_simples(projet, d$y[[1]], test), d$y[[1]]$transform))
  p <- as.vector(stats::predict(m$ajustement, X))
  p[vides] <- NA
  e <- p - y
  ok <- !is.na(e)
  obs <- data.frame(uid = test, nom = noms, hors_domaine = FALSE, stringsAsFactors = FALSE)
  obs[[paste(m$y$nom, "observé")]] <- y
  obs[[paste(m$y$nom, "prédit")]] <- p
  list(n = length(test), hors_domaine = 0, observations = obs,
       statistiques = data.frame(y = m$y$nom, n = sum(ok), RMSEP = if (any(ok)) sqrt(mean(e[ok]^2)) else NA_real_,
                                 biais = if (any(ok)) mean(e[ok]) else NA_real_, SEP = if (sum(ok) > 1) stats::sd(e[ok]) else NA_real_,
                                 Q2ext = if (any(ok)) 1 - sum(e[ok]^2) / sum((y[ok] - mean(m$donnees$.y))^2) else NA_real_, stringsAsFactors = FALSE))
}

# La stabilité de la première coupure : des tirages bootstrap des groupes (avec remise ; chaque
# groupe tiré est un pli de la validation croisée interne, deux tirages du même groupe faisant
# deux plis), l'arbre refait et élagué sur chacun. Pour chaque variable, le nombre et la part des
# tirages où elle fait la première coupure, et son seuil médian ; graine fixe (2026).
stabilite_arbre <- function(m) avec_graine(2026, {
  methode <- if (m$classement) "class" else "anova"
  us <- unique(m$groupes)
  aucune <- ""  # (aucune coupure : dit dans la langue de ce qui l'affiche, stabilite_affichee)
  tirages <- lapply(seq_len(m$reglages_arbre$tirages), function(i) {
    ss <- us[sample.int(length(us), length(us), replace = TRUE)]
    k <- unlist(lapply(ss, function(s) which(m$groupes == s)))
    plis <- rep(seq_along(ss), vapply(ss, function(s) sum(m$groupes == s), 0L))
    a <- tryCatch(arbre_elague(m$donnees[k, , drop = FALSE], plis, methode, m$controle, m$reglages_arbre$elagage)$arbre, error = function(e) NULL)
    if (is.null(a)) return(list(variable = NA_character_, seuil = NA_real_))
    p <- premiere_coupure(a, m$variables)
    if (is.na(p$variable)) p$variable <- aucune
    p
  })
  v <- vapply(tirages, function(x) x$variable, "")
  s <- vapply(tirages, function(x) as.numeric(x$seuil), 0)
  quoi <- sort(unique(v[!is.na(v)]), method = "radix")  # (le même ordre partout, quelle que soit la langue du système)
  out <- data.frame(variable = quoi, tirages = vapply(quoi, function(x) sum(v == x, na.rm = TRUE), 0),
                    seuil_median = vapply(quoi, function(x) { z <- s[!is.na(v) & v == x]; if (all(is.na(z))) NA_real_ else stats::median(z, na.rm = TRUE) }, 0),
                    stringsAsFactors = FALSE, row.names = NULL)
  out$frequence <- 100 * out$tirages / length(tirages)
  out <- out[order(-out$tirages), c("variable", "tirages", "frequence", "seuil_median"), drop = FALSE]
  rownames(out) <- NULL
  attr(out, "echecs") <- sum(is.na(v))
  out
})

# La stabilité pour l'interface, Exporter ou un texte : « (aucune coupure) » dans la langue voulue.
stabilite_affichee <- function(s, fr = !identical(getOption("nuee.langue"), "en")) {
  if (is.null(s)) return(NULL)
  s$variable[!is.na(s$variable) & !nzchar(s$variable)] <- if (fr) "(aucune coupure)" else "(no split)"
  s
}

# La feuille (son numéro parmi les feuilles) de chaque observation de l'arbre, ou de chaque ligne
# de `X` (des X bruts) en suivant les coupures et leurs substitutions comme predict.rpart.
feuilles_observations <- function(a) match(a$where, which(a$frame$var == "<leaf>"))
feuilles_nouvelles <- function(a, X) {
  if (!nrow(X)) return(integer(0))
  b <- a
  b$frame$yval <- seq_len(nrow(a$frame))
  match(as.integer(stats::predict(b, X, type = "vector")), which(a$frame$var == "<leaf>"))
}

# Les observations d'un arbre : réponse observée, ajustée et prédite en validation croisée (et
# probabilités), résidu standardisé (par le RMSEE) et son quantile normal, feuille, groupe.
observations_arbre <- function(projet, m) {
  obs <- data.frame(uid = m$observations, nom = unname(projet$noms_obs[as.character(m$observations)]), stringsAsFactors = FALSE)
  if (m$classement) {
    obs$classe <- as.character(m$donnees$.y)
    obs[["classe ajustée"]] <- m$classification$ajuste$predite
    obs[["classe prédite (VC)"]] <- m$classification$vc$predite
    for (k in m$classes) obs[[paste("probabilité", k)]] <- unname(m$proba[, k])
  } else {
    y <- m$y$nom
    obs[[paste(y, "observé")]] <- m$donnees$.y
    obs[[paste(y, "ajusté")]] <- m$ajuste
    obs[[paste(y, "prédit (VC)")]] <- m$vc$prediction
    e <- m$donnees$.y - m$ajuste
    rmsee <- sqrt(mean(e^2))
    obs$residu <- if (rmsee > 0) e / rmsee else rep(0, length(e))
    obs$quantile_normal <- stats::qnorm((rank(obs$residu, ties.method = "first") - 0.375) / (length(e) + 0.25))
  }
  obs$feuille <- feuilles_observations(m$ajustement)
  obs$groupe_vc <- m$groupes
  obs
}

resultat_tree <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  a <- m$ajustement
  y <- m$y$nom
  regles <- regles_arbre(a, m$classes)
  ct <- m$complet$cptable
  i <- which.min(ct[, "xerror"])
  rmsee <- if (!m$classement) sqrt(mean((m$donnees$.y - m$ajuste)^2))
  qualite <- if (m$classement) list(bien_classes = m$classification$ajuste$global, bien_classes_vc = m$classification$vc$global)
             else list(r2 = m$r2, Q2 = m$vc$Q2, RMSEcv = m$vc$RMSEcv, RMSEE = rmsee)
  feuilles <- nrow(regles)
  chiffres <- if (m$classement) sprintf("%s %s · %d %s", fmt_pourcent(m$classification$vc$global), traduire("bien classées (VC)", "correct (CV)"),
                                        feuilles, traduire("feuilles", "leaves"))
              else sprintf("R2 %.2f · Q2 %.2f · %d %s", m$r2, m$vc$Q2, feuilles, traduire("feuilles", "leaves"))
  list(id = id, projet = p$id, nom = d$name %||% "", titre = d$title %||% "", type = "TREE", analyse = TRUE, prediction = TRUE,
       classement = m$classement, regression = FALSE, opls = FALSE, Ap = 0, A = 0, N = m$N, K = m$K, noms_y = I(y),
       transformation_y = nom_transformation(m$y$transform) %||% "", observations = observations_arbre(p, m),
       arbre = noeuds_arbre(a, m$variables, m$classes), regles = regles, importance = importance_arbre(a, m$variables),
       elagage = list(table = elagage_arbre(m), cp = m$cp, feuilles = feuilles, regle = m$reglages_arbre$elagage,
                      seuil = unname(ct[i, "xerror"] + ct[i, "xstd"]), minimum = unname(ct[i, "xerror"])),
       premiere = premiere_coupure(a, m$variables), reglages_arbre = m$reglages_arbre, classes = I(m$classes %||% character(0)),
       classification = m$classification, qualite = qualite,
       RMSEE = if (!m$classement) stats::setNames(list(rmsee), y), RMSEcv = if (!m$classement) stats::setNames(list(m$vc$RMSEcv), y),
       comparaison = if (!m$classement) list(R2 = m$r2, Q2 = m$vc$Q2, RMSEE = rmsee, RMSEcv = m$vc$RMSEcv),
       stabilite = stabilite_affichee(m$stabilite), exclues_manquantes = length(m$exclues), manquantes_x = m$manquantes_x,
       minbucket_utilise = m$controle$minbucket,
       variables_x = I(vapply(m$variables, `[[`, "", "nom")),
       G = m$G, vc_par = m$vc_par %||% "", reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app),
       note = d$note %||% "", test = m$test, demande = d$demande %||% list(type = d$type, numero = d$number),
       vues_defaut = I(c("arbre", if (m$classement) "classement" else "obspred")), chiffres = chiffres)
}

# La stabilité de la première coupure, calculée à la demande (bootstrap des groupes) et gardée
# avec le modèle.
demande_analyse_arbre_stabilite <- function(q) {
  e <- modele(q$modele)
  if (!identical(e$modele$type, "TREE")) stop(traduire("Seulement pour un arbre de décision.", "Only for a decision tree."), call. = FALSE)
  if (is.null(e$modele$stabilite)) etat$modeles[[q$modele]]$modele$stabilite <- stabilite_arbre(e$modele)
  s <- etat$modeles[[q$modele]]$modele$stabilite
  list(modele = q$modele, stabilite = stabilite_affichee(s), tirages = e$modele$reglages_arbre$tirages, echecs = attr(s, "echecs") %||% 0)
}

predire_tree <- function(projet, m, d, observations) {
  X <- x_bruts(projet, m$variables, observations)
  vides <- rowSums(!is.na(X)) == 0
  out <- data.frame(row.names = seq_along(observations))
  if (m$classement) {
    P <- proba_arbre(m$ajustement, X, m$classes)
    P[vides, ] <- NA
    out[["classe prédite"]] <- classe_la_plus_probable(P, m$classes)
    for (k in m$classes) out[[paste("probabilité", k)]] <- unname(P[, k])
    if (length(projet$variables)) out[["classe observée"]] <- as.character(classe_observations(projet, d$classe, observations))
  } else {
    p <- as.vector(stats::predict(m$ajustement, X))
    p[vides] <- NA
    out[[paste(m$y$nom, "prédit")]] <- p
    y <- suppressWarnings(transformer(valeurs_simples(projet, d$y[[1]], observations), d$y[[1]]$transform))
    if (any(!is.na(y))) out[[paste(m$y$nom, "observé")]] <- y
  }
  f <- feuilles_nouvelles(m$ajustement, X)
  f[vides] <- NA
  out$feuille <- f
  out$hors_domaine <- FALSE
  rownames(out) <- NULL
  out
}

tables_tree <- function(projet, m) {
  a <- m$ajustement
  obs <- observations_arbre(projet, m)
  obs$uid <- NULL
  names(obs)[names(obs) == "nom"] <- "Observation"
  p <- premiere_coupure(a, m$variables)
  resume <- data.frame(N = m$N, variables = m$K, groupes_vc = m$G, feuilles = sum(a$frame$var == "<leaf>"), cp = m$cp,
                       premiere_coupure = p$regle, stringsAsFactors = FALSE)
  if (m$classement) {
    resume$bien_classees <- m$classification$ajuste$global
    resume$bien_classees_vc <- m$classification$vc$global
  } else {
    resume$R2 <- m$r2
    resume$Q2 <- m$vc$Q2
    resume$RMSEcv <- m$vc$RMSEcv
  }
  out <- list(resume = resume, observations = obs, regles = regles_arbre(a, m$classes),
              importance = importance_arbre(a, m$variables), elagage = elagage_arbre(m))
  if (m$classement) {
    out$classement <- tableau_classes(m$classification$ajuste$table, m$classes)
    out$classement_vc <- tableau_classes(m$classification$vc$table, m$classes)
  }
  if (!is.null(m$stabilite)) out$stabilite <- stabilite_affichee(m$stabilite)
  out
}

# Un tableau de classement pour Exporter : la classe observée, puis une colonne par classe prédite.
tableau_classes <- function(tab, classes) {
  out <- data.frame(classe = classes, matrix(as.numeric(tab), length(classes)), check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[-1] <- classes
  out
}

texte_methodes_tree <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  r <- m$reglages_arbre
  a <- m$ajustement
  g <- function(x) guillemets_texte(x, fr)
  noms <- vapply(m$variables, `[[`, "", "nom")
  feuilles <- sum(a$frame$var == "<leaf>")
  laissees <- if (length(m$exclues)) sprintf(if (fr) " (%d autres laissées de côté : leur réponse ou toutes leurs valeurs X manquent)"
                                             else " (%d others left out: their response or all their X values are missing)", length(m$exclues)) else ""
  reponse <- if (m$classement) sprintf(if (fr) "la classe %s (%d classes)" else "the class %s (%d classes)", g(m$y$nom), length(m$classes))
             else if (!is.null(m$y$transform)) formule_transformation(m$y$transform, g(m$y$nom), fr) else g(m$y$nom)
  p1 <- sprintf(if (fr) "L'arbre porte sur %s%s ; sa réponse est %s." else "The tree uses %s%s; its response is %s.",
                observations_texte(m$N, d, fr), laissees, reponse)
  explicatives <- if (length(noms) <= 12) sprintf(if (fr) "%d variables explicatives (%s)" else "%d explanatory variables (%s)", length(noms), liste_mots(noms, fr))
                  else sprintf(if (fr) "%d variables explicatives" else "%d explanatory variables", length(noms))
  p2 <- sprintf(if (fr) "Un arbre de %s a été construit avec rpart (Therneau & Atkinson ; Breiman et al., 1984) sur les valeurs brutes de %s, pour que ses seuils se lisent dans leurs unités ; les valeurs manquantes (%d observations en ont au moins une) sont prises en charge par des variables de substitution (%d au plus par coupure). Chaque feuille compte au moins %d observations%s (un nœud n'est coupé que s'il en compte au moins %d) et l'arbre a au plus %d niveaux (paramètre de complexité initial %s)."
                else "A %s tree was built with rpart (Therneau & Atkinson; Breiman et al., 1984) on the raw values of %s, so that its thresholds read in their units; missing values (%d observations have at least one) are handled by surrogate variables (%d at most per split). Each leaf holds at least %d observations%s (a node is split only if it holds at least %d) and the tree has at most %d levels (initial complexity parameter %s).",
                if (m$classement) (if (fr) "classement (indice de Gini)" else "classification (Gini index)") else (if (fr) "régression" else "regression"),
                explicatives, m$manquantes_x, r$maxsurrogate, m$controle$minbucket,
                if (m$controle$minbucket < r$minbucket) sprintf(if (fr) " (%d demandées, ramenées au sixième des %d observations pour que l'arbre puisse se couper)"
                                                                else " (%d requested, lowered to a sixth of the %d observations so that the tree can split)", r$minbucket, m$N) else "",
                m$controle$minsplit, r$maxdepth, format(r$cp, decimal.mark = if (fr) "," else "."))
  p3 <- sprintf(if (fr) "Sa taille a été choisie par une validation croisée interne %s : %s. Sa capacité de prédiction a été estimée par une validation croisée externe, chaque groupe étant prédit par un arbre refait entièrement sans lui, sa validation croisée interne et son élagage compris%s."
                else "Its size was chosen by an internal cross-validation %s: %s. Its predictive ability was estimated by an external cross-validation, each group being predicted by a tree entirely rebuilt without it, its internal cross-validation and pruning included%s.",
                groupes_texte(m$vc_par, m$G, fr),
                if (r$elagage == "1se") (if (fr) "l'arbre retenu est le plus petit dont l'erreur de validation croisée ne dépasse pas l'erreur minimale de plus d'une erreur-type (règle 1-ET ; Breiman et al., 1984)"
                                         else "the tree retained is the smallest whose cross-validated error is within one standard error of the minimum (1-SE rule; Breiman et al., 1984)")
                else (if (fr) "l'arbre retenu est celui de l'erreur de validation croisée minimale" else "the tree retained is that of the minimum cross-validated error"),
                if (m$classement) "" else " (Q² = 1 − PRESS / SS)")
  premiere <- premiere_coupure(a, m$variables)
  coupure <- if (!is.na(premiere$regle)) sprintf(if (fr) ", la première coupure étant %s" else ", the first split being %s", premiere$regle) else ""
  n_feuilles <- if (feuilles == 1) (if (fr) "une seule feuille" else "a single leaf") else sprintf(if (fr) "%d feuilles" else "%d leaves", feuilles)
  p4 <- if (m$classement) {
    sprintf(if (fr) "L'arbre retenu a %s%s ; il classe bien %s des observations, %s en validation croisée."
            else "The tree retained has %s%s; it classifies %s of the observations correctly, %s in cross-validation.",
            n_feuilles, coupure, pourcent_texte(m$classification$ajuste$global, fr), pourcent_texte(m$classification$vc$global, fr))
  } else {
    sprintf(if (fr) "L'arbre retenu a %s%s ; R² = %s, Q² = %s et RMSEcv = %s."
            else "The tree retained has %s%s; R² = %s, Q² = %s and RMSEcv = %s.",
            n_feuilles, coupure, nombre_texte(m$r2, 2, fr), nombre_texte(m$vc$Q2, 2, fr), format(signif(m$vc$RMSEcv, 4), decimal.mark = if (fr) "," else "."))
  }
  s <- m$stabilite
  if (!is.null(s) && nrow(s)) {
    p4 <- paste(p4, if (!nzchar(s$variable[1])) {
      sprintf(if (fr) "Sur %d tirages bootstrap des groupes, l'arbre élagué n'a aucune coupure dans %s des tirages."
              else "Over %d bootstrap draws of the groups, the pruned tree has no split in %s of the draws.", r$tirages, pourcent_texte(s$frequence[1] / 100, fr))
    } else {
      sprintf(if (fr) "Sur %d tirages bootstrap des groupes, la première coupure porte sur %s dans %s des tirages%s."
              else "Over %d bootstrap draws of the groups, the first split is on %s in %s of the draws%s.",
              r$tirages, g(s$variable[1]), pourcent_texte(s$frequence[1] / 100, fr),
              if (!is.na(s$seuil_median[1])) sprintf(if (fr) " (seuil médian %s)" else " (median threshold %s)", format(signif(s$seuil_median[1], 4), decimal.mark = if (fr) "," else ".")) else "")
    })
  }
  if (!is.null(m$test)) {
    p4 <- paste(p4, if (m$classement) sprintf(if (fr) "Un jeu de test de %d observations, laissées hors de l'arbre et de la validation croisée, a ensuite été prédit : %s bien classées%s."
                                               else "A test set of %d observations, left out of the tree and of the cross-validation, was then predicted: %s correctly classified%s.",
                                               m$test$n, pourcent_texte(m$test$classement$bien_classees, fr), classes_inconnues_texte(m$test$classement$inconnues, fr))
                else sprintf(if (fr) "Un jeu de test de %d observations, laissées hors de l'arbre et de la validation croisée, a ensuite été prédit : RMSEP = %s, biais = %s, Q² externe = %s."
                             else "A test set of %d observations, left out of the tree and of the cross-validation, was then predicted: RMSEP = %s, bias = %s, external Q² = %s.",
                             m$test$n, nombre_texte(m$test$statistiques$RMSEP, 3, fr), nombre_texte(m$test$statistiques$biais, 3, fr), nombre_texte(m$test$statistiques$Q2ext, 2, fr)))
  }
  logiciels <- references_classement(c("breiman", "rpart"), "rpart", fr, version)
  list(paragraphes = c(p1, p2, p3, p4, logiciels$phrase), references = logiciels$references)
}
