# Formules R des régressions (Nouveau modèle) : la formule des réglages d'un modèle, écrite comme
# on l'écrit dans R (lm, mgcv, et lme4 pour les effets aléatoires), et une formule lue en réglages.
#   note ~ sucre * fermete + log10(acidite) + I(sucre^2) + s(polyphenols, k = 5) + offset(log(effort)) + (1 | site/secteur)
# Une formule lue n'est jamais évaluée : R la découpe (str2lang), puis seuls y sont acceptés des
# noms de variables ou d'identifiants du projet, des nombres et quelques fonctions, chacune
# reconnue à sa place. Elle devient les réglages de Nouveau modèle (rôles, transformations,
# termes croisés, courbes, effets aléatoires, effort) et n'est pas gardée : un projet reçu ne
# porte pas de code.

# Un nom tel qu'une formule l'écrit : entre accents graves s'il n'est pas un nom de R.
nom_formule <- function(n) if (identical(make.names(n), n)) n else paste0("`", gsub("`", "\\`", n, fixed = TRUE), "`")

nombre_formule <- function(v) format(v, digits = 15)

# Une transformation (lire_transformation) appliquée à `x`, en R.
texte_transformation <- function(tr, x) {
  if (is.null(tr)) return(x)
  p <- as.numeric(tr$params)
  type <- as.character(tr$type)
  nb <- nombre_formule
  affine <- function() {  # C1·x + C2
    s <- if (p[2] == 1) x else if (p[2] == -1) paste0("-", x) else sprintf("%s * %s", nb(p[2]), x)
    if (p[3] > 0) sprintf("%s + %s", s, nb(p[3])) else if (p[3] < 0) sprintf("%s - %s", s, nb(-p[3])) else s
  }
  switch(type,
    log10 = sprintf("log10(%s)", affine()),
    ln = sprintf("log(%s)", affine()),
    neglog = sprintf("I(-log10(%s))", affine()),
    exp = sprintf("exp(%s)", affine()),
    lineaire = if (p[2] == 1 && p[3] == 0) x else sprintf("I(%s)", affine()),
    logit = if (p[2] == 0) sprintf("log(%s / (%s - %s))", x, nb(p[3]), x) else sprintf("log((%s - %s) / (%s - %s))", x, nb(p[2]), nb(p[3]), x),
    "7" = if (p[2] == 1 && p[3] == 0) {
      if (p[4] == 1) x else if (p[4] == 0.5) sprintf("sqrt(%s)", x) else sprintf("I(%s^%s)", x, nb(p[4]))
    } else sprintf("I((%s)^%s)", affine(), nb(p[4])),
    stop(sprintf(traduire("Transformation sans formule R : %s", "Transformation without an R formula: %s"), type), call. = FALSE))
}

# La formule des réglages `r` d'une régression, tels que Nouveau modèle les envoie (type, x, y,
# transformations par numéro de variable, croises, lisses et leur k_courbes et par_courbes,
# aleatoires, decalage). Sans Y, une formule sans réponse (« ~ x1 + x2 »).
formule_reglages <- function(projet, r) {
  nom_variable <- function(u) as.character(variable_du_projet(projet, u)$ids[1])
  transformations <- r$transformations %||% list()
  terme <- function(u) {
    n <- transformations[[as.character(u)]]
    tr <- if (length(n) && nzchar(n[1]) && !identical(n[1], "autre")) lire_transformation(as.character(n[1]))
    texte_transformation(tr, nom_formule(nom_variable(u)))
  }
  croises <- r$croises
  if (is.matrix(croises)) croises <- lapply(seq_len(nrow(croises)), function(i) croises[i, ])
  type <- as.character(r$type %||% "LM")[1]
  lisses <- if (identical(type, "GAM")) as.numeric(unlist(r$lisses)) else numeric(0)
  droite <- c(
    vapply(as.numeric(unlist(r$x)), function(u) {
      if (!u %in% lisses) return(terme(u))
      k <- suppressWarnings(as.integer(unlist(r$k_courbes[[as.character(u)]])))
      f <- suppressWarnings(as.numeric(unlist(r$par_courbes[[as.character(u)]])))  # (une courbe par catégorie de f)
      sprintf("s(%s)", paste(c(terme(u), if (length(f) && !is.na(f[1])) sprintf("by = %s", nom_formule(nom_variable(f[1]))),
                               if (length(k) && !is.na(k[1])) sprintf("k = %d", k[1])), collapse = ", "))
    }, ""),
    vapply(croises %||% list(), function(c) {
      c <- as.numeric(c)
      n <- table(factor(c, levels = unique(c)))
      paste(vapply(names(n), function(u) if (n[[u]] == 1) terme(u) else sprintf("I(%s^%d)", terme(u), n[[u]]), ""), collapse = ":")
    }, ""),
    if (length(r$decalage) && type %in% GENERALISES) sprintf("offset(log(%s))", nom_formule(nom_variable(as.numeric(r$decalage)[1]))),
    {
      # (comme lme4 : (1 | site/secteur) ; avec des pentes aléatoires, (1 + annee | site/secteur) quand
      # chaque niveau a les mêmes, sinon un terme par niveau, (1 + annee | site) + (1 | site:secteur))
      a <- if (type %in% c("LMM", GENERALISES)) specs_groupement(r$aleatoires) else list()
      groupes <- vapply(a, function(s) nom_formule(nom_groupement(projet, s)), "")
      gauches <- vapply(a, function(s) paste(c("1", vapply(as.numeric(s$pentes %||% numeric(0)), terme, "")), collapse = " + "), "")
      if (length(a) && all(gauches == gauches[1])) sprintf("(%s | %s)", gauches[1], paste(groupes, collapse = "/"))
      else vapply(seq_along(a), function(l) sprintf("(%s | %s)", gauches[l], paste(groupes[seq_len(l)], collapse = ":")), "")
    })
  y <- as.numeric(unlist(r$y))
  paste(if (length(y)) paste0(terme(y[1]), " "), "~ ", if (length(droite)) paste(droite, collapse = " + ") else "1", sep = "")
}

# Les réglages d'une formule R (texte), pour une régression de type `type` et de loi `famille` :
# un modèle à effets aléatoires devient mixte (LM → LMM), sans eux linéaire (LMM → LM), une courbe
# en fait un modèle additif (→ GAM). Une erreur qui dit ce qui ne va pas, sinon list(type, x, y,
# transformations, croises, lisses, k_courbes, par_courbes, aleatoires, decalage).
lire_formule <- function(projet, texte, type = "LM", famille = NULL) {
  err <- function(fr, en, ...) stop(sprintf(traduire(fr, en), ...), call. = FALSE)
  texte <- paste(as.character(texte), collapse = " ")
  if (nchar(texte) > 5000) err("Cette formule est trop longue.", "This formula is too long.")
  f <- tryCatch(str2lang(texte), error = function(e) err("Formule illisible : %s", "Unreadable formula: %s", conditionMessage(e)))
  if (!is.call(f) || !identical(f[[1]], as.name("~")) || length(f) != 3) {
    err("Une formule s'écrit : réponse ~ termes, par exemple note ~ sucre + fermete.", "A formula reads: response ~ terms, for example note ~ sugar + firmness.")
  }
  noms <- vapply(projet$variables, function(v) as.character(v$ids[1]), "")
  ids <- as.character(projet$observation_id_names %||% character(0))[-1]
  ids <- ids[nzchar(ids) & !startsWith(ids, "$")]
  texte_de <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")
  refus <- function(e) err("« %s » n'a pas sa place dans une formule ici : des variables, des nombres, log, log10, log1p, exp, sqrt, I(), s(), offset(log()), (1 | groupe), « : » et « * ».",
                           "\"%s\" has no place in a formula here: variables, numbers, log, log10, log1p, exp, sqrt, I(), s(), offset(log()), (1 | group), \":\" and \"*\".", texte_de(e))
  fonction <- function(e) if (is.call(e) && is.name(e[[1]])) as.character(e[[1]]) else ""
  variable <- function(e) {
    n <- as.character(e)
    k <- match(n, noms)
    if (is.na(k)) k <- match(nom_comparable(n), nom_comparable(noms))
    if (is.na(k)) err("« %s » n'est pas une variable du projet.", "\"%s\" is not a variable of the project.", n)
    k
  }
  # un nombre : 3, -0.5, (2)
  nombre <- function(e) {
    if (is.numeric(e) && length(e) == 1 && is.finite(e)) return(as.numeric(e))
    if (fonction(e) == "(" && length(e) == 2) return(nombre(e[[2]]))
    if (fonction(e) == "-" && length(e) == 2) { v <- nombre(e[[2]]); return(if (is.null(v)) NULL else -v) }
    NULL
  }
  # a·x + b, x une variable : list(k, a, b), ou NULL
  affine <- function(e) {
    if (is.name(e)) return(list(k = variable(e), a = 1, b = 0))
    f <- fonction(e)
    if (f == "(" && length(e) == 2) return(affine(e[[2]]))
    if (f == "-" && length(e) == 2) { v <- affine(e[[2]]); return(if (is.null(v)) NULL else list(k = v$k, a = -v$a, b = -v$b)) }
    if (f %in% c("*", "/", "+", "-") && length(e) == 3) {
      n1 <- nombre(e[[2]])
      n2 <- nombre(e[[3]])
      v1 <- if (is.null(n1)) affine(e[[2]])
      v2 <- if (is.null(n2)) affine(e[[3]])
      if (f == "*" && !is.null(n1) && !is.null(v2)) return(list(k = v2$k, a = n1 * v2$a, b = n1 * v2$b))
      if (f == "*" && !is.null(v1) && !is.null(n2)) return(list(k = v1$k, a = n2 * v1$a, b = n2 * v1$b))
      if (f == "/" && !is.null(v1) && !is.null(n2) && n2 != 0) return(list(k = v1$k, a = v1$a / n2, b = v1$b / n2))
      if (f == "+" && !is.null(v1) && !is.null(n2)) return(list(k = v1$k, a = v1$a, b = v1$b + n2))
      if (f == "+" && !is.null(n1) && !is.null(v2)) return(list(k = v2$k, a = v2$a, b = v2$b + n1))
      if (f == "-" && !is.null(v1) && !is.null(n2)) return(list(k = v1$k, a = v1$a, b = v1$b - n2))
      if (f == "-" && !is.null(n1) && !is.null(v2)) return(list(k = v2$k, a = -v2$a, b = n1 - v2$b))
    }
    NULL
  }
  nb <- nombre_formule
  code_affine <- function(type, v) if (v$a == 1 && v$b == 0) type else sprintf("%s:%s:%s", type, nb(v$a), nb(v$b))
  # un terme simple : une variable, telle quelle ou transformée → list(k, tr) (tr : le code de
  # lire_transformation, ou NULL) ; I(x^2), I(log10(x)^3)... → list(k, tr, puissance)
  simple <- function(e) {
    if (is.name(e)) return(list(k = variable(e), tr = NULL))
    f <- fonction(e)
    un <- function(g) {
      if (length(e) != 2 || !is.null(names(e)) && any(nzchar(names(e)))) refus(e)
      v <- affine(e[[2]])
      if (is.null(v)) refus(e)
      g(v)
    }
    switch(f,
      "(" = if (length(e) == 2) simple(e[[2]]) else refus(e),
      log10 = un(function(v) list(k = v$k, tr = code_affine("log10", v))),
      exp = un(function(v) list(k = v$k, tr = sprintf("exp:%s:%s", nb(v$a), nb(v$b)))),
      log1p = un(function(v) list(k = v$k, tr = sprintf("ln:%s:%s", nb(v$a), nb(v$b + 1)))),
      sqrt = un(function(v) list(k = v$k, tr = if (v$a == 1 && v$b == 0) "racine2" else sprintf("puissance:%s:%s:0.5", nb(v$a), nb(v$b)))),
      log = {
        if (length(e) != 2) refus(e)
        a <- e[[2]]
        while (fonction(a) == "(" && length(a) == 2) a <- a[[2]]
        if (fonction(a) == "/" && length(a) == 3) {  # le logit entre deux bornes : log((x - C1) / (C2 - x))
          num <- affine(a[[2]])
          den <- affine(a[[3]])
          if (!is.null(num) && !is.null(den) && num$k == den$k && num$a == 1 && den$a == -1) {
            return(list(k = num$k, tr = sprintf("logit:%s:%s", nb(-num$b), nb(den$b))))
          }
          refus(e)
        }
        un(function(v) list(k = v$k, tr = sprintf("ln:%s:%s", nb(v$a), nb(v$b))))
      },
      I = {
        if (length(e) != 2) refus(e)
        a <- e[[2]]
        g <- fonction(a)
        if (g == "-" && length(a) == 2 && fonction(a[[2]]) == "log10") {
          v <- simple(a[[2]])
          return(list(k = v$k, tr = sub("^log10", "neglog", if (identical(v$tr, "log10")) "log10:1:0" else v$tr)))
        }
        if (g == "^" && length(a) == 3) {
          p <- nombre(a[[3]])
          if (is.null(p)) refus(e)
          v <- affine(a[[2]])
          if (!is.null(v) && (v$a != 1 || v$b != 0)) return(list(k = v$k, tr = sprintf("puissance:%s:%s:%s", nb(v$a), nb(v$b), nb(p))))
          base <- simple(a[[2]])
          if (p >= 2 && p <= 3 && p == round(p)) return(c(base, list(puissance = p)))
          if (!is.null(base$tr)) refus(e)
          return(list(k = base$k, tr = if (p == 0.5) "racine2" else if (p == 0.25) "racine4" else sprintf("puissance:1:0:%s", nb(p))))
        }
        if (g %in% c("log10", "log", "exp", "sqrt", "log1p", "(")) return(simple(a))
        v <- affine(a)
        if (is.null(v)) refus(e)
        list(k = v$k, tr = if (v$a == 1 && v$b == 0) NULL else sprintf("lineaire:%s:%s", nb(v$a), nb(v$b)))
      },
      refus(e))
  }
  groupement <- function(e) {
    n <- as.character(e)
    if (n %in% ids) return(list(by = "id", id = n))
    k <- match(n, noms)
    if (is.na(k)) k <- match(nom_comparable(n), nom_comparable(noms))
    if (is.na(k)) err("« %s » n'est ni un identifiant des observations ni une variable du projet.", "\"%s\" is neither an observation ID nor a variable of the project.", n)
    list(by = "variable", variable = as.numeric(projet$variables[[k]]$uid))
  }
  # les termes séparés par « + »
  termes <- function(e) if (fonction(e) == "+" && length(e) == 3) c(termes(e[[2]]), termes(e[[3]])) else list(e)
  # une pente aléatoire sans son ordonnée à l'origine, ou à part d'elle (|| ; 0 + x ; (1 | site) +
  # (0 + x | site)) : des effets indépendants, que Nuée ne prend pas (un modèle qui dépend alors de
  # l'endroit où x vaut 0)
  independantes <- function(t) {
    err("« %s » : une pente aléatoire indépendante de l'ordonnée à l'origine (||, 0 + x, ou un terme à part de (1 | groupe)) ne se prend pas : Nuée estime chaque pente aléatoire avec sa corrélation à l'ordonnée à l'origine de son groupe, (1 + x | groupe), un modèle qui ne dépend pas de l'endroit où x vaut 0.",
        "\"%s\": a random slope independent of the intercept (||, 0 + x, or a term apart from (1 | group)) isn't taken: Nuée estimates each random slope with its correlation to its group's intercept, (1 + x | group), a model that doesn't depend on where x is 0.", texte_de(t))
  }
  # le côté gauche d'un effet aléatoire (1, 1 + annee, annee : lme4 y met l'ordonnée à l'origine) :
  # ses pentes, des termes simples (transformés ou non : simple) ; l'ordonnée à l'origine ôtée
  # (0 + x, x - 1) est refusée. `t` : le terme entier, pour le message.
  gauche_aleatoire <- function(e, t) {
    if (fonction(e) == "-" && length(e) == 3 && identical(nombre(e[[3]]), 1)) independantes(t)
    pentes <- list()
    for (x in termes(e)) {
      n <- nombre(x)
      if (identical(n, 1)) next
      if (!is.null(n)) { if (n %in% c(0, -1)) independantes(t) else refus(x) }
      s <- simple(x)
      if (!is.null(s$puissance)) refus(x)
      pentes[[length(pentes) + 1]] <- s
    }
    pentes
  }
  # ce que la formule demande
  principaux <- list()  # par variable (son indice) : sa transformation
  courbes <- integer(0)
  croises <- list()     # des vecteurs d'indices, un par terme croisé
  puissances <- list()  # I(x^2) seuls : un terme croisé si x est aussi un terme principal, sinon x^2
  aleatoires <- list()
  barres <- list()      # les termes (… | groupe) : leur côté gauche (gauche_aleatoire) et leurs groupes
  decalage <- NULL
  k_courbes <- list()   # par variable (son indice) : la dimension demandée de sa courbe
  par_courbes <- list() # par variable (son indice) : la variable qualitative d'une courbe par catégorie (son indice)
  ajouter <- function(s) {
    cle <- as.character(s$k)
    if (!is.null(principaux[[cle]]) && !identical(principaux[[cle]]$tr, s$tr)) {
      err("« %s » entre deux fois dans la formule, transformée de deux façons : une variable n'a qu'une transformation.",
          "\"%s\" enters the formula twice, transformed two ways: a variable has a single transformation.", noms[s$k])
    }
    principaux[[cle]] <<- list(k = s$k, tr = s$tr)
  }
  facteurs <- function(e) if (fonction(e) == ":" && length(e) == 3) c(facteurs(e[[2]]), facteurs(e[[3]])) else list(e)
  qualitative <- function(k) length(projet$variables[[k]]$categories) > 0
  croiser <- function(fs) {
    ks <- unlist(lapply(fs, function(s) rep(s$k, s$puissance %||% 1)))
    # (une variable qualitative une fois au plus : ni son carré, ni a:b:a)
    if (any(duplicated(ks) & vapply(ks, qualitative, TRUE))) err("%s", "%s", refus_qualitative_double(noms[ks]))
    croises[[length(croises) + 1]] <<- list(ks = ks, fs = fs)
  }
  for (t in termes(f[[3]])) {
    ft <- fonction(t)
    if (!is.null(nombre(t))) {
      if (nombre(t) == 1) next
      err("Nuée garde toujours l'ordonnée à l'origine : « + 0 » ou « - 1 » n'y ont pas de sens.", "Nuée always keeps the intercept: \"+ 0\" or \"- 1\" don't apply.")
    }
    if (ft == "-") err("Retirer un terme (« - x ») : ne l'écrivez pas. Nuée garde toujours l'ordonnée à l'origine.", "To remove a term (\"- x\"), leave it out. Nuée always keeps the intercept.")
    if (ft == "(" && length(t) == 2 && fonction(t[[2]]) %in% c("|", "||")) {  # un effet aléatoire, lu après la boucle
      if (fonction(t[[2]]) == "||") independantes(t)
      barres[[length(barres) + 1]] <- list(gauche = gauche_aleatoire(t[[2]][[2]], t), droite = t[[2]][[3]])
      next
    }
    if (ft == "(" && length(t) == 2) {
      if (fonction(t[[2]]) == "+") err("Écrivez les termes sans parenthèses : a + b.", "Write the terms without brackets: a + b.")
      t <- t[[2]]
      ft <- fonction(t)
    }
    if (ft == "offset") {
      a <- if (length(t) == 2) t[[2]]
      if (fonction(a) != "log" || length(a) != 2 || !is.name(a[[2]])) {
        err("L'effort s'écrit offset(log(effort)).", "The effort reads offset(log(effort)).")
      }
      decalage <- variable(a[[2]])
      next
    }
    if (ft == "s") {  # s(x), s(x, k = 5) : sa dimension de base ; s(x, by = f) : une courbe par catégorie de f
      arguments <- as.list(t)[-1]
      noms_args <- names(arguments) %||% rep("", length(arguments))
      if (!length(arguments) || nzchar(noms_args[1]) || !all(noms_args[-1] %in% c("k", "by")) || anyDuplicated(noms_args[-1])) {
        err("Une courbe s'écrit s(x), s(x, k = 5) pour fixer sa dimension, ou s(x, by = f) pour une courbe par catégorie de la variable qualitative f.",
            "A curve reads s(x), s(x, k = 5) to set its dimension, or s(x, by = f) for a curve per category of the qualitative variable f.")
      }
      k_s <- NULL
      if ("k" %in% noms_args) {
        k_s <- nombre(arguments[["k"]])
        if (is.null(k_s) || k_s < 3 || k_s > 100 || k_s != round(k_s)) {
          err("La dimension d'une courbe s'écrit k = un nombre entier de 3 à 100 : s(x, k = 5).", "A curve's dimension reads k = a whole number from 3 to 100: s(x, k = 5).")
        }
      }
      s <- simple(arguments[[1]])
      if (!is.null(s$puissance)) refus(t)
      if ("by" %in% noms_args) {
        if (!is.name(arguments[["by"]])) refus(t)
        f_s <- variable(arguments[["by"]])
        if (!qualitative(f_s)) {
          err("« %s » est quantitative : une courbe par catégorie, s(x, by = f), se fait avec une variable qualitative (Nuée ne prend pas un coefficient qui varie avec une variable quantitative).",
              "\"%s\" is quantitative: a curve per category, s(x, by = f), is made with a qualitative variable (Nuée doesn't take a coefficient varying with a quantitative variable).", noms[f_s])
        }
        par_courbes[[as.character(s$k)]] <- f_s
      }
      ajouter(s)
      courbes <- c(courbes, s$k)
      if (!is.null(k_s)) k_courbes[[as.character(s$k)]] <- as.integer(k_s)
      next
    }
    if (ft == "*") {  # a*b*c : a, b, c et tous leurs termes croisés
      etoiles <- function(e) if (fonction(e) == "*" && length(e) == 3) c(etoiles(e[[2]]), etoiles(e[[3]])) else list(e)
      fs <- lapply(etoiles(t), simple)
      for (s in fs) if (is.null(s$puissance)) ajouter(s) else refus(t)
      n <- length(fs)
      for (taille in seq_len(n)[-1]) for (cmb in utils::combn(n, taille, simplify = FALSE)) croiser(fs[cmb])
      next
    }
    if (ft == ":") {
      croiser(lapply(facteurs(t), simple))
      next
    }
    if (ft %in% c("^", "/", "%in%", "|")) {
      err("« %s » : les puissances s'écrivent I(x^2), les termes croisés a:b ou a*b.", "\"%s\": powers read I(x^2), cross terms a:b or a*b.", texte_de(t))
    }
    s <- simple(t)
    if (!is.null(s$puissance)) puissances[[length(puissances) + 1]] <- s else ajouter(s)
  }
  # x² seul : une transformation de x ; avec x : un terme croisé x*x
  for (s in puissances) {
    if (is.null(principaux[[as.character(s$k)]]) && is.null(s$tr)) {
      ajouter(list(k = s$k, tr = sprintf("puissance:1:0:%s", nb(s$puissance))))
    } else croiser(list(s))
  }
  # (un facteur d'un terme croisé sans effet principal, a:b sans a : il n'entre que par ses termes
  # croisés, comme y ~ secteur + secteur:annee dans R, une pente par secteur ; sa transformation est
  # la même dans chacun)
  seuls <- list()
  for (c in croises) for (s in c$fs) {
    cle <- as.character(s$k)
    p <- principaux[[cle]] %||% seuls[[cle]]
    if (is.null(p)) {
      seuls[[cle]] <- list(k = s$k, tr = s$tr)
      next
    }
    if (!identical(p$tr, s$tr)) {
      err("« %s » est transformée d'une autre façon dans le terme croisé.", "\"%s\" is transformed another way in the cross term.", noms[s$k])
    }
  }
  # (la variable d'une courbe par catégorie est aussi un terme : son effet principal, sans lequel
  # chaque catégorie aurait la même moyenne, les courbes de mgcv étant centrées)
  for (cle in names(par_courbes)) {
    f_s <- par_courbes[[cle]]
    if (is.null(principaux[[as.character(f_s)]])) {
      err("« %s » a une courbe par catégorie de « %s » : « %s » doit être aussi un terme de la formule, son effet principal (y ~ %s + s(%s, by = %s)). Chaque courbe de mgcv est centrée : sans lui, toutes les catégories auraient la même moyenne.",
          "\"%s\" has a curve per category of \"%s\": \"%s\" must also be a term of the formula, its main effect (y ~ %s + s(%s, by = %s)). Each mgcv curve is centred: without it, every category would have the same mean.",
          noms[as.integer(cle)], noms[f_s], noms[f_s], noms[f_s], noms[as.integer(cle)], noms[f_s])
    }
  }
  # les effets aléatoires, emboîtés du plus large au plus fin : un seul terme, (1 + annee | site/secteur)
  # (ses pentes à chaque niveau, comme lme4 le lit), ou un terme par niveau, (1 + annee | site) +
  # (1 | site:secteur) ; chaque pente aléatoire est aussi un terme fixe, transformé de même
  emboitement <- function() err("Les effets aléatoires de Nuée sont emboîtés, du plus large au plus fin : (1 | site/secteur), ou un terme par niveau, (1 + annee | site) + (1 | site:secteur).",
                                "Nuée's random effects are nested, the widest first: (1 | site/sector), or one term per level, (1 + year | site) + (1 | site:sector).")
  chaine <- function(e, op) if (fonction(e) == op && length(e) == 3) c(chaine(e[[2]], op), chaine(e[[3]], op)) else list(e)
  niveau <- function(g, pentes) {
    if (!is.name(g)) refus(g)
    for (s in pentes) {
      if (length(projet$variables[[s$k]]$categories)) err("« %s » est qualitative : une pente aléatoire se fait sur une variable quantitative.",
                                                         "\"%s\" is qualitative: a random slope is made on a quantitative variable.", noms[s$k])
      p <- principaux[[as.character(s$k)]]
      if (is.null(p) || !identical(p$tr, s$tr)) {
        err("« %s » a une pente aléatoire : elle doit être aussi un terme fixe de la formule, transformée de la même façon (y ~ annee + (1 + annee | site)).",
            "\"%s\" has a random slope: it must also be a fixed term of the formula, transformed the same way (y ~ year + (1 + year | site)).", noms[s$k])
      }
      if (s$k %in% courbes) err("« %s » a une pente aléatoire : elle ne peut pas être aussi une courbe (une pente aléatoire varie autour d'une pente fixe, une droite).",
                                "\"%s\" has a random slope: it can't also be a curve (a random slope varies around a fixed slope, a straight line).", noms[s$k])
    }
    c(groupement(g), if (length(pentes)) list(pentes = unique(vapply(pentes, function(s) s$k, 0))))
  }
  if (length(barres) == 1 && fonction(barres[[1]]$droite) != ":") {
    aleatoires <- lapply(chaine(barres[[1]]$droite, "/"), niveau, barres[[1]]$gauche)
  } else {
    for (l in seq_along(barres)) {
      g <- chaine(barres[[l]]$droite, ":")
      if (length(g) != l || !all(vapply(g, is.name, TRUE)) ||
          (l > 1 && !identical(vapply(g[-l], as.character, ""), vapply(chaine(barres[[l - 1]]$droite, ":"), as.character, "")))) emboitement()
      aleatoires[[l]] <- niveau(g[[l]], barres[[l]]$gauche)
    }
  }
  # la réponse
  reponse <- simple(f[[2]])
  if (!is.null(reponse$puissance)) reponse <- list(k = reponse$k, tr = sprintf("puissance:1:0:%s", nb(reponse$puissance)))
  if (!is.null(principaux[[as.character(reponse$k)]]) || !is.null(seuls[[as.character(reponse$k)]])) {
    err("« %s » est la réponse : elle ne peut pas être aussi un terme.", "\"%s\" is the response: it can't also be a term.", noms[reponse$k])
  }
  for (s in c(principaux, seuls, list(reponse))) {
    if (qualitative(s$k) && !is.null(s$tr)) err("« %s » est qualitative : elle ne se transforme pas.", "\"%s\" is qualitative: it can't be transformed.", noms[s$k])
  }
  if (qualitative(reponse$k)) err("La réponse « %s » est qualitative : une régression demande une réponse quantitative.", "The response \"%s\" is qualitative: a regression needs a quantitative response.", noms[reponse$k])
  for (k in courbes) if (qualitative(k)) err("Une courbe va avec une variable quantitative, pas « %s ».", "A curve goes with a quantitative variable, not \"%s\".", noms[k])
  if (!is.null(decalage) && qualitative(decalage)) err("L'effort est une variable quantitative.", "The effort is a quantitative variable.")
  # les lectures (crochets, $, chaînes...) sont refusées plus haut ; les numéros des variables
  uid <- function(k) vapply(k, function(i) as.numeric(projet$variables[[i]]$uid), 0)
  type <- as.character(type %||% "LM")[1]
  if (!type %in% REGRESSIONS) type <- "LM"
  if (length(courbes) && type != "GAM") type <- "GAM"
  if (length(aleatoires) && type == "LM") type <- "LMM"
  if (!length(aleatoires) && type == "LMM") type <- "LM"
  if (!is.null(decalage) && !(type %in% GENERALISES && as.character(famille %||% "")[1] %in% FAMILLES_LOG)) {
    err("L'effort (offset) va avec un GLM ou un GAM de loi Poisson, binomiale négative, gamma ou Tweedie : choisissez d'abord le type et la loi de Y.",
        "The effort (offset) goes with a GLM or GAM with a Poisson, negative binomial, gamma or Tweedie distribution: choose the type and the distribution of Y first.")
  }
  transformations <- stats::setNames(list(), character(0))
  for (s in c(principaux, seuls, list(reponse))) if (!is.null(s$tr)) transformations[[as.character(uid(s$k))]] <- s$tr
  croises_uid <- unique(lapply(croises, function(c) uid(c$ks)))
  list(type = type, x = I(vapply(principaux, function(s) uid(s$k), 0, USE.NAMES = FALSE)), y = I(uid(reponse$k)),
       transformations = transformations, croises = I(croises_uid), lisses = I(vapply(courbes, uid, 0)),
       k_courbes = stats::setNames(k_courbes, vapply(as.integer(names(k_courbes)), uid, 0)),
       par_courbes = stats::setNames(lapply(par_courbes, uid), vapply(as.integer(names(par_courbes)), uid, 0)),
       aleatoires = I(lapply(aleatoires, function(a) { if (length(a$pentes)) a$pentes <- I(uid(a$pentes)); a })),
       decalage = if (!is.null(decalage)) uid(decalage))
}
