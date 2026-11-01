# Nouvelles analyses : importer un tableau (texte, Excel, R, SPSS, Stata, SAS, jamovi...),
# définir un modèle, choisir automatiquement le nombre de composantes, enregistrer un projet.

# Un tableau (une ligne par observation) en projet ; formats.R le lit (`table` : la feuille
# Excel ou l'objet R à lire). La première colonne sert d'identifiant si c'est du texte, ou si
# son nom l'annonce (« ID », « Obs », « Nom »...) et que ses valeurs sont toutes différentes ;
# les colonnes de texte et les facteurs deviennent des variables qualitatives (une catégorie par
# valeur distincte), sauf les colonnes de texte dont toutes les valeurs diffèrent, gardées en
# identifiants secondaires si `secondaires` (pas pour un fichier à prédire : quelques lignes
# peuvent avoir chacune une catégorie différente).
importer_tableau <- function(chemin, table = NULL, secondaires = TRUE) {
  projet_depuis_tableau(lire_tableau(chemin, table), basename(chemin), secondaires)
}

# Un tableau collé (copié dans un tableur) : du texte, une ligne par observation, des colonnes
# séparées par des tabulations.
importer_texte <- function(texte, nom = "tableau", secondaires = TRUE) {
  projet_depuis_tableau(vers_data_frame(lire_texte_delimite(enc2utf8(texte))), nom, secondaires)
}

# `toutes_modalites` : garder aussi les modalités d'un facteur sans observation (script R
# autonome : les catégories restent celles du projet d'origine).
projet_depuis_tableau <- function(df, nom = "tableau", secondaires = TRUE, toutes_modalites = FALSE) {
  if (!ncol(df) || !nrow(df)) stop(traduire("Le tableau est vide", "The table is empty"))
  premiere_id <- is.character(df[[1]]) || (anyDuplicated(df[[1]]) == 0 &&
    (is.factor(df[[1]]) || grepl("^(primary )?id$|^obs|^nom|^name", names(df)[1], ignore.case = TRUE)))
  ids <- if (premiere_id) as.character(df[[1]]) else as.character(seq_len(nrow(df)))
  if (premiere_id) df <- df[-1]
  # Une colonne de texte dont toutes les valeurs diffèrent (un code par observation) n'est
  # pas une variable : on la garde en identifiant secondaire, pour les étiquettes.
  secondaire <- vapply(df, function(x) secondaires && (is.character(x) || is.factor(x)) && !anyNA(x) && anyDuplicated(x) == 0, TRUE)
  ids_secondaires <- lapply(df[secondaire], as.character)
  df <- df[!secondaire]
  vars <- list()
  donnees <- matrix(NA_real_, nrow(df), ncol(df))
  for (k in seq_along(df)) {
    x <- df[[k]]
    v <- list(uid = k, ids = names(df)[k], categories = NULL, formula = "")
    if (is.numeric(x)) {
      donnees[, k] <- x
    } else {
      cats <- if (is.factor(x)) levels(if (toutes_modalites) x else droplevels(x)) else sort(unique(stats::na.omit(as.character(x))))
      v$categories <- cats
      donnees[, k] <- match(as.character(x), cats)
    }
    vars[[k]] <- v
  }
  dimnames(donnees) <- list(as.character(seq_len(nrow(df))), as.character(seq_along(df)))
  p <- list(format = "usp-json/1", source = nom,
            observation_id_names = c("Observation", names(ids_secondaires)), variable_id_names = "Variable",
            observations = lapply(seq_len(nrow(df)), function(i) {
              list(uid = i, ids = c(ids[i], vapply(ids_secondaires, `[`, "", i, USE.NAMES = FALSE)))
            }),
            variables = vars,
            datasets = list(list(name = nom, observations = seq_len(nrow(df)), variables = seq_along(df))),
            models = list(), prediction_sets = list(), dendrograms = list(), donnees = donnees)
  p$noms_obs <- stats::setNames(ids, as.character(seq_len(nrow(df))))
  p$noms_var <- stats::setNames(names(df), as.character(seq_along(df)))
  class(p) <- "projet_nuee"
  p
}

# Transformations proposées pour une variable, et leur nom quand on relit un modèle.
TRANSFORMATIONS <- list(
  racine2 = list(type = 7, params = c(NA, 1, 0, 0.5)),
  racine4 = list(type = 7, params = c(NA, 1, 0, 0.25)),
  log10 = list(type = "log10", params = c(NA, 1, 0)))

nom_transformation <- function(tr) {
  if (is.null(tr)) return(NULL)
  p <- tr$params
  if (identical(as.character(tr$type), "log10") && isTRUE(p[2] == 1)) {
    return(if (isTRUE(p[3] == 0)) "log10" else sprintf("log10:%s", format(p[3], digits = 15)))
  }
  if (identical(as.character(tr$type), "7") && isTRUE(p[2] == 1 && p[3] == 0)) {
    if (p[4] == 1) return(NULL)
    if (p[4] == 0.5) return("racine2")
    if (p[4] == 0.25) return("racine4")
  }
  "autre"
}

variable_du_projet <- function(projet, uid) {
  for (v in projet$variables) if (identical(as.numeric(v$uid), as.numeric(uid))) return(v)
  stop(traduire("Variable introuvable : ", "Variable not found: "), uid)
}

# Les termes d'une variable : un seul pour une variable quantitative, un par catégorie
# (indicatrice 0/1) pour une variable qualitative.
termes_variable <- function(projet, uid, echelle = "UV", transformation = NULL) {
  v <- variable_du_projet(projet, uid)
  nom <- as.character(v$ids[1])
  if (length(v$categories)) {
    return(lapply(seq_along(v$categories), function(k) list(
      variables = as.numeric(uid), categories = k, lags = integer(0),
      name = sprintf("%s(%s)", nom, v$categories[k]), transform = NULL,
      scaling = list(type = echelle))))
  }
  list(list(variables = as.numeric(uid), categories = 0, lags = integer(0), name = nom,
            transform = transformation, scaling = list(type = echelle)))
}

# Définition d'un modèle.
#   type : "PCA", "PLS", "PLS-DA", "OPLS" ou "OPLS-DA" (opls.R : autant de composantes
#   prédictives que de Y, ou que de classes moins une) ; x, y : numéros des variables ; classe : variable
#   qualitative dont les catégories sont les classes d'une PLS-DA ; observations : numéros
#   des observations retenues (toutes par défaut) ; echelle : "UV", "Par", "Ctr" ou "none",
#   pour toutes les variables, ou un vecteur nommé par numéro de variable (l'élément
#   "defaut", sinon UV, vaut pour les autres) ;
#   transformations : vecteur nommé par numéro de variable, "racine2", "racine4", "log10" ou
#   "log10:C2" pour log10(x + C2) ;
#   croises : termes croisés et carrés, une paire de numéros de variables X par terme,
#   par exemple list(c(3, 5), c(4, 4)) pour v3*v5 et v4*v4 ;
#   spectres : prétraitement des spectres (spectres.R), par exemple
#   list(list(type = "snv"), list(type = "sg", derivee = 1, fenetre = 11, degre = 2)) ; les
#   variables X sont alors prises dans l'ordre du tableau ;
#   test : numéros des observations d'un jeu de test (jeu_test.R) : hors du modèle, puis
#   prédites par lui.
definir_modele <- function(projet, type, x, y = NULL, classe = NULL, observations = NULL,
                           echelle = "UV", A = NULL, nom = NULL, titre = "", G = 7,
                           transformations = NULL, croises = NULL, spectres = NULL, test = NULL) {
  ech <- function(uid) {
    if (is.null(names(echelle))) return(echelle[[1]])
    if (as.character(uid) %in% names(echelle)) return(echelle[[as.character(uid)]])
    if ("defaut" %in% names(echelle)) echelle[["defaut"]] else "UV"
  }
  trans <- function(uid) {
    n <- if (as.character(uid) %in% names(transformations)) transformations[[as.character(uid)]]
    if (is.null(n) || !nzchar(n)) return(NULL)
    # log10(x + C2), écrit "log10:C2" (transformation proposée pour une variable avec des 0)
    if (startsWith(n, "log10:")) return(list(type = "log10", params = c(NA, 1, as.numeric(sub("^log10:", "", n)))))
    TRANSFORMATIONS[[n]] %||% stop(traduire("Transformation inconnue : ", "Unknown transformation: "), n)
  }
  obs <- observations %||% vapply(projet$observations, function(o) as.numeric(o$uid), 0)
  test <- unique(as.numeric(test))
  obs <- setdiff(obs, test)
  x <- unique(as.numeric(x))
  y <- unique(as.numeric(y))
  spectres <- etapes_spectres(spectres)
  if (length(spectres)) x <- x[order(match(x, vapply(projet$variables, function(v) as.numeric(v$uid), 0)))]
  plsda <- type %in% c("PLS-DA", "OPLS-DA")
  opls <- type %in% c("OPLS", "OPLS-DA")
  if (plsda) {
    if (is.null(classe)) stop(traduire("Une PLS-DA demande une variable de classe", "A PLS-DA needs a class variable"))
    y <- NULL
    yt <- termes_variable(projet, classe, "UV")
    # seules les observations dont la classe est connue entrent dans le modèle
    obs <- obs[!is.na(colonne(projet, classe, obs))]
  }
  Ap <- if (opls) (if (plsda) length(unique(colonne(projet, classe, obs))) - 1 else length(y))
  if (opls && (is.na(Ap) || Ap < 1)) stop(if (plsda) traduire("Une OPLS-DA demande au moins deux classes", "An OPLS-DA needs at least two classes") else traduire("Une OPLS demande au moins une variable Y", "An OPLS needs at least one Y variable"))
  if (opls && !is.null(A) && A > 0) A <- max(A, Ap)
  d <- list(type = if (type == "PCA") "PCA" else "PLS", plsda = plsda, opls = opls,
            name = nom %||% paste0("M", length(projet$models) + 1), title = titre,
            observations = obs, A = A %||% 0, cv_groups = G,
            x = c(unlist(lapply(x, function(u) termes_variable(projet, u, ech(u), trans(u))), recursive = FALSE),
                  unlist(lapply(croises, function(paire) termes_croises(projet, paire, x, ech("defaut"))), recursive = FALSE)),
            y = if (plsda) yt else unlist(lapply(y, function(u) termes_variable(projet, u, ech(u), trans(u))), recursive = FALSE))
  if (plsda) d$classe <- classe
  if (opls) d$Ap <- Ap
  if (length(spectres)) d$spectres <- spectres
  if (length(test)) d$test <- test
  d
}

# Termes croisés de deux variables X (ou carré d'une variable X quantitative). Une variable
# qualitative y entre par chacune de ses catégories : variete*culture donne
# variete(Gala)*culture(bio), variete(Gala)*culture(conventionnel)... Chaque terme est le produit de ses facteurs déjà
# centrés et mis à l'échelle (voir construire_bloc), puis il est lui-même mis à l'échelle.
termes_croises <- function(projet, paire, x, echelle = "UV") {
  paire <- as.numeric(paire)
  if (!all(paire %in% x)) stop(traduire("Un terme croisé ne peut utiliser que des variables X", "A cross term can only use X variables"))
  vs <- lapply(paire, function(u) variable_du_projet(projet, u))
  if (paire[1] == paire[2] && length(vs[[1]]$categories)) {
    stop(traduire("Pas de carré pour une variable qualitative : ", "No square for a qualitative variable: "), vs[[1]]$ids[1])
  }
  options <- lapply(vs, function(v) if (length(v$categories)) seq_along(v$categories) else 0)
  combinaisons <- expand.grid(options)
  lapply(seq_len(nrow(combinaisons)), function(i) {
    cats <- as.numeric(combinaisons[i, ])
    noms <- vapply(seq_along(vs), function(j) {
      nom <- as.character(vs[[j]]$ids[1])
      if (cats[j]) sprintf("%s(%s)", nom, vs[[j]]$categories[cats[j]]) else nom
    }, "")
    list(variables = paire, categories = cats, lags = integer(0), name = paste(noms, collapse = "*"),
         transform = NULL, scaling = list(type = echelle))
  })
}

# Les réglages d'un modèle, sous la forme de definir_modele() (pour les modifier) :
# variables X et Y, classe, observations exclues, échelle et transformation par variable.
reglages_modele <- function(projet, d) {
  simples <- function(termes) unique(unlist(lapply(termes, function(t)
    if (length(t$variables) == 1) as.numeric(t$variables))))
  echelles <- list()
  transformations <- list()
  for (t in c(d$x, d$y)) {
    if (length(t$variables) != 1) next
    k <- as.character(t$variables)
    echelles[[k]] <- t$scaling$type %||% "UV"
    tr <- nom_transformation(t$transform)
    if (!is.null(tr)) transformations[[k]] <- tr
  }
  tous <- vapply(projet$observations, function(o) as.numeric(o$uid), 0)
  if (isTRUE(d$plsda) && is.null(d$classe)) {
    # PLS-DA venant d'un fichier .usp : usp_extract a reconnu la variable qui définit ses classes
    # (« PLS-DA classes match variable variete »)
    nom <- sub("^PLS-DA classes match variable ", "", d[["reference"]][["note"]] %||% "")
    v <- Filter(function(v) identical(as.character(v$ids[1]), nom) && length(v$categories), projet$variables)
    if (length(v)) d$classe <- v[[1]]$uid
  }
  plsda <- isTRUE(d$plsda) && !is.null(d$classe)
  list(type = paste0(if (isTRUE(d$opls)) "O" else "", if (plsda) "PLS-DA" else d$type), x = I(simples(d$x) %||% numeric(0)),
       y = I(if (plsda) numeric(0) else simples(d$y) %||% numeric(0)), classe = if (plsda) d$classe else NULL,
       exclues = I(setdiff(tous, c(d$observations, d$test))), echelles = echelles,
       transformations = transformations, A = d$A, G = d$cv_groups %||% 7,
       croises = unique(lapply(Filter(function(t) length(t$variables) > 1, d$x), function(t) as.numeric(t$variables))),
       spectres = d$spectres %||% list(), test = I(as.numeric(d$test %||% numeric(0))), nom = d$name %||% "", titre = d$title %||% "")
}

# Nombre de composantes significatives, avec les règles publiées de l'ajustement automatique :
#   R1 : Q2 > limite (PLS : 0 si plus de 100 observations, 0,05 sinon ; ACP : la règle dit
#        seulement qu'elle croît avec les composantes « pour tenir compte de la perte de
#        degrés de liberté » ; on prend 1 - (N-a-1)(K-a) / ((N-a)(K-a+1)), notre lecture :
#        la valeur exacte n'est pas publiée) ;
#   R2 : Q2V > limite pour au moins 20 % des variables (de Y en PLS, de X en ACP) si elles
#        sont plus de 25, sinon pour au moins racine(M) d'entre elles (ACP : la formule
#        publiée, si la valeur propre dépasse 1,5 ou K < 30) ;
#   N3 : pas plus de min(N, K) / 2 composantes en ACP ;
#   N4 : composante non significative si elle explique moins de 1 % (ACP) de X ou de Y (PLS)
#        et qu'aucune variable n'en gagne plus de 2 % (ACP) / 3 % (PLS).
# Les composantes sont calculées une à une et le calcul s'arrête à la première qui n'est pas
# significative.
autoajuster <- function(projet, definition, A_max = NULL) {
  N <- length(definition$observations)
  K <- length(definition$x)
  pls <- identical(definition$type, "PLS")
  if (isTRUE(definition$opls)) {
    # OPLS : les composantes prédictives, plus autant de composantes orthogonales que la
    # validation croisée en justifie (règle R1 : chacune doit améliorer la prédiction de Y)
    Ap <- definition$Ap %||% 1
    Ao_max <- max(0, min(8, K - Ap, N - Ap - 2))
    m <- ajuster(projet, definition, A = Ao_max + Ap)
    limite <- if (N > 100) 0 else 0.05
    Ao <- 0
    while (Ao < Ao_max && m$q2[Ap + Ao + 1] > limite) Ao <- Ao + 1
    return(Ao + Ap)
  }
  A_max <- A_max %||% max(1, min(floor(min(N, K) / 2), 15))
  if (pls) A_max <- max(1, min(A_max, K, N - 2))
  premiere_non_significative <- NA
  significative <- function(res, a) {
    bloc_ss <- if (pls) res$ssyv else res$ssxv
    nv <- ncol(res$press)
    limite <- if (pls) (if (N > 100) 0 else 0.05) else 1 - (N - a - 1) * (K - a) / ((N - a) * (K - a + 1))
    q2 <- 1 - sum(res$press[a, ]) / sum(res$ss_vc[a, ])
    q2v <- 1 - res$press[a, ] / res$ss_vc[a, ]
    besoin <- if (nv > 25) 0.2 * nv else if (pls) sqrt(nv) else sqrt(nv) * log10(max(10, nv - 20))
    ssx <- colSums(res$ssxv)
    vp <- (ssx[a] - ssx[a + 1]) / ssx[1] * min(N, K)  # valeur propre
    r1 <- q2 > limite
    # na.rm : une colonne sans variance (catégorie absente des observations retenues) n'a pas de Q2V
    r2 <- sum(q2v > limite, na.rm = TRUE) >= besoin && (pls || vp > 1.5 || K < 30)
    gain <- (bloc_ss[, a] - bloc_ss[, a + 1]) / bloc_ss[, 1]
    n4 <- sum(bloc_ss[, a] - bloc_ss[, a + 1]) / sum(bloc_ss[, 1]) < 0.01 &&
      !any(gain > if (pls) 0.03 else 0.02, na.rm = TRUE)
    (r1 || r2) && !n4
  }
  arret <- function(res, a) {
    if (significative(res, a)) return(FALSE)
    premiere_non_significative <<- a
    TRUE
  }
  m <- ajuster(projet, definition, A = A_max, arret = arret)
  # (m$A : moins que A_max quand il n'y avait plus de variation à décrire)
  if (is.na(premiere_non_significative)) m$A else premiere_non_significative - 1
}

# Enregistre un projet (données et définitions de modèles) au format lu par lire_projet().
enregistrer_projet <- function(projet, chemin) {
  p <- unclass(projet)
  for (i in seq_along(p$datasets)) {
    d <- p$datasets[[i]]
    p$datasets[[i]]$values <- unname(projet$donnees[as.character(d$observations), as.character(d$variables), drop = FALSE])
  }
  p$donnees <- p$noms_obs <- p$noms_var <- NULL
  p$id <- p$chemin <- p$chemin_enregistrement <- p$modifie <- p$table <- NULL  # état de la session
  jsonlite::write_json(p, chemin, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  invisible(chemin)
}
