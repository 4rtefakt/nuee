# Les observations d'un modèle choisies par une condition (Nouveau modèle › onglet Observations,
# « Garder seulement les observations où… ») : une expression R écrite comme une formule de
# Nouvelle variable, par exemple site == "S1" & annee >= 2010, !is.na(densite) ou
# habitat %in% c("marais", "prairie"). Elle est gardée dans la définition du modèle (d$condition)
# avec les observations qu'il prendrait sans elle (d$cochees : les cases cochées, sans le jeu de
# test ; une PLS-DA, sans les observations de classe inconnue), et refaite à chaque ajustement sur
# les données du moment (resoudre_observations) : d$observations devient les observations de
# d$cochees où elle est vraie (une valeur manquante compte pour fausse). Les cases cochées ne
# changent pas : retirer la condition rend les observations qu'elle écartait. Le jeu de test garde
# ses propres cases. Les observations écartées sont hors du modèle, comme celles qu'on décoche :
# Prédire les observations les prédit.
#
# Gardée dans le fichier du projet et refaite à chaque ajustement (à la réouverture d'un projet
# reçu aussi), une condition ne doit pas pouvoir exécuter autre chose qu'elle-même : avant d'être
# évaluée, elle est lue (lire_condition), et n'y sont acceptés que les noms des variables et des
# identifiants du projet, des nombres, du texte et les fonctions de FONCTIONS_CONDITION (formule.R
# lit de même les formules des régressions sans les exécuter). Une formule de Nouvelle variable,
# tapée et calculée une fois, n'est pas gardée : elle n'a pas cette limite.

# Les fonctions qu'une condition peut appeler : les opérateurs, des fonctions de R qui ne lisent ni
# n'écrivent rien d'autre que leurs arguments, et celles des formules de Nouvelle variable
# (variables.R : moyenne_groupe, precedent...).
FONCTIONS_CONDITION <- c(
  "(", "!", "&", "|", "xor", "==", "!=", "<", "<=", ">", ">=", "%in%", "+", "-", "*", "/", "^", "%%", "%/%", ":",
  "c", "is.na", "ifelse", "abs", "sqrt", "exp", "log", "log10", "log2", "log1p", "round", "floor", "ceiling", "trunc", "signif",
  "as.numeric", "as.character", "nchar", "substr", "substring", "startsWith", "endsWith", "tolower", "toupper", "trimws",
  "paste", "paste0", "mean", "median", "min", "max", "sum", "quantile", "pmin", "pmax", "interaction",
  "moyenne_groupe", "ecart_groupe", "group_mean", "group_deviation",
  "precedent", "suivant", "croissance", "previous", "following", "growth_rate")

# (celles de Nuée : le script de l'export vers R les écrit, code_fonctions_condition)
FONCTIONS_NUEE_CONDITION <- c("moyenne_groupe", "ecart_groupe", "group_mean", "group_deviation",
                              "precedent", "suivant", "croissance", "previous", "following", "growth_rate")

# (des noms de R que lit une condition quand aucune variable ne les porte)
CONSTANTES_CONDITION <- c("T", "F", "pi")

# Une condition est-elle donnée ? (d$condition absente ou vide : non)
avec_condition <- function(d) length(d$condition) > 0 && nzchar(texte_condition(d$condition))

# Le texte d'une condition, d'un seul tenant (une liste ou des morceaux relus d'un fichier).
texte_condition <- function(x) trimws(paste(as.character(unlist(x)), collapse = " "))

# Les noms qu'une condition lit, tels que environnement_formule() les donne (variables.R) : les
# variables du projet qui ont des données (la dernière quand deux portent le même nom), puis les
# identifiants des observations qu'aucune variable ne cache. list(variables : leurs numéros, nommés
# par leur nom ; ids : les noms des identifiants).
noms_condition <- function(projet) {
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(projet$donnees), projet$variables)
  noms <- vapply(vars, function(v) as.character(v$ids[1]), "")
  uids <- vapply(vars, function(v) as.numeric(v$uid), 0)
  garde <- !duplicated(noms, fromLast = TRUE)
  variables <- stats::setNames(uids[garde], noms[garde])
  ids <- as.character(projet$observation_id_names %||% character(0))[-1]
  ids <- unique(ids[nzchar(ids) & !startsWith(ids, "$") & !ids %in% names(variables)])
  list(variables = variables, ids = ids)
}

# Une condition lue sans être exécutée : l'expression R, après avoir vérifié qu'elle n'appelle que
# des fonctions de FONCTIONS_CONDITION, sur des noms de variables ou d'identifiants du projet, des
# nombres et du texte. Sinon, une erreur qui dit ce qui ne va pas.
lire_condition <- function(projet, texte) {
  err <- function(fr, en, ...) stop(sprintf(traduire(fr, en), ...), call. = FALSE)
  texte <- texte_condition(texte)
  if (nchar(texte) > 5000) err("Cette condition est trop longue.", "This condition is too long.")
  e <- tryCatch(parse(text = texte, keep.source = FALSE, encoding = "UTF-8"),
                error = function(x) err("Condition illisible : %s", "Unreadable condition: %s", conditionMessage(x)))
  if (length(e) != 1) err("Une condition est une seule expression, sans « ; » : site == \"S1\" & annee >= 2010.",
                          "A condition is a single expression, without \";\": site == \"S1\" & year >= 2010.")
  n <- noms_condition(projet)
  noms <- c(names(n$variables), n$ids)
  voir <- function(x) {
    if (is.call(x)) {
      f <- x[[1]]
      if (!is.name(f) || !as.character(f) %in% FONCTIONS_CONDITION) {
        quoi <- if (!is.name(f)) paste(deparse(f, width.cutoff = 500L), collapse = " ")
                else if (identical(make.names(as.character(f)), as.character(f))) paste0(as.character(f), "()") else as.character(f)
        err("« %s » n'est pas permis dans une condition : des comparaisons (==, !=, <, >=…), &, |, !, %%in%%, is.na(), ifelse(), des calculs (+, log10()…) et les fonctions des formules de Nouvelle variable (moyenne_groupe(), precedent()…).",
            "\"%s\" isn't allowed in a condition: comparisons (==, !=, <, >=…), &, |, !, %%in%%, is.na(), ifelse(), arithmetic (+, log10()…) and the functions of New variable formulas (group_mean(), previous()…).",
            quoi)
      }
      arguments <- as.list(x)[-1]
      for (i in seq_along(arguments)) {
        # (un argument vide, c(1, , 2) : le symbole vide, qu'on ne peut pas passer plus loin)
        if (identical(arguments[[i]], quote(expr = ))) err("Condition incomplète : il y manque un argument.", "Incomplete condition: an argument is missing.")
        voir(arguments[[i]])
      }
    } else if (is.name(x)) {
      nom <- as.character(x)
      if (!nom %in% noms && !nom %in% CONSTANTES_CONDITION) stop(variable_introuvable(nom), call. = FALSE)
    } else if (!is.null(x) && !is.atomic(x)) {
      err("« %s » n'a pas sa place dans une condition.", "\"%s\" has no place in a condition.", paste(deparse(x, width.cutoff = 500L), collapse = " "))
    }
  }
  voir(e[[1]])
  e[[1]]
}

# La valeur d'une condition pour chaque observation du projet, dans l'ordre des lignes de
# projet$donnees : TRUE ou FALSE (une valeur manquante compte pour FALSE). Évaluée sur toutes les
# observations, comme une formule de Nouvelle variable (environnement_formule) ; elle doit donner
# une valeur logique (ou 0 et 1) par observation. attr(, "avertissement") : R a averti en
# l'évaluant (une valeur de texte lue comme un nombre...) ; ses avertissements ne sont pas montrés.
valeurs_condition <- function(projet, condition) {
  err <- function(fr, en, ...) stop(sprintf(traduire(fr, en), ...), call. = FALSE)
  e <- lire_condition(projet, condition)
  env <- environnement_formule(projet, all.names(e))  # (les seules variables qu'elle lit)
  avertit <- FALSE
  r <- tryCatch(withCallingHandlers(eval(e, envir = env), warning = function(w) {
    avertit <<- TRUE
    invokeRestart("muffleWarning")
  }), error = function(x) err("Condition : %s", "Condition: %s", conditionMessage(x)))
  n <- nrow(projet$donnees)
  doit <- traduire("La condition doit donner TRUE ou FALSE (ou 1 et 0) pour chaque observation", "The condition must give TRUE or FALSE (or 1 and 0) for each observation")
  if (is.factor(r) || is.character(r)) err("%s ; elle donne du texte.", "%s; it gives text.", doit)
  if (is.numeric(r)) {
    autres <- r[!is.na(r) & r != 0 & r != 1]
    if (length(autres)) err("%s ; elle donne d'autres nombres (%s).", "%s; it gives other numbers (%s).", doit, format(autres[1], digits = 6))
    r <- r == 1
  }
  if (!is.logical(r)) err("%s ; elle donne %s.", "%s; it gives %s.", doit, if (is.null(r)) "NULL" else class(r)[1])
  if (length(r) != n) err("La condition doit donner une valeur par observation (%d) ; elle en donne %d.",
                          "The condition must give one value per observation (%d); it gives %d.", n, length(r))
  r <- as.vector(r)
  r[is.na(r)] <- FALSE
  structure(r, avertissement = avertit)
}

# Les observations de `observations` (des numéros) où la condition est vraie, dans leur ordre ;
# une erreur quand il n'y en a aucune.
observations_condition <- function(projet, condition, observations) {
  vraie <- valeurs_condition(projet, condition)
  garde <- vraie[match(as.character(observations), rownames(projet$donnees))]
  garde[is.na(garde)] <- FALSE
  if (!any(garde)) {
    stop(sprintf(traduire("Aucune des %d observations cochées ne remplit la condition %s.", "None of the %d ticked observations meets the condition %s."),
                 length(observations), texte_condition(condition)), call. = FALSE)
  }
  as.numeric(observations)[garde]
}

# La définition `d` avec les observations de sa condition d'après les données du moment : celles
# de d$cochees où elle est vraie ; telle quelle sans condition. Chaque ajustement passe par là
# (modele.R : ajuster ; serveur.R : ajuster_et_garder), pour tous les types de modèles.
resoudre_observations <- function(projet, d) {
  if (!avec_condition(d)) return(d)
  d$condition <- texte_condition(d$condition)
  d$cochees <- as.numeric(unlist(d$cochees %||% d$observations))
  d$observations <- observations_condition(projet, d$condition, d$cochees)
  d
}

# Les observations d'un modèle en mots, pour le texte des Méthodes : « 312 observations », ou avec
# une condition « les 312 observations où site == "S1" & annee >= 2010 » (fr : en français) ; avec
# `de` : « de 312 observations », « des 312 observations où… » (« le tableau de… »).
observations_texte <- function(n, d, fr, de = FALSE) {
  if (!avec_condition(d)) return(sprintf(if (!de) "%d observations" else if (fr) "de %d observations" else "of %d observations", n))
  sprintf(if (fr) (if (de) "des %d observations où %s" else "les %d observations où %s") else if (de) "of the %d observations where %s" else "the %d observations where %s",
          n, gsub("\\s*\n\\s*", " ", texte_condition(d$condition)))
}

# ------------------------------------------------------------ l'export vers R

# Les identifiants des observations que lit la condition de `d` : le tableau du script les ajoute
# à ses colonnes (script.R : tableau_script).
ids_condition <- function(projet, d) {
  if (!avec_condition(d)) return(character(0))
  intersect(all.names(lire_condition(projet, d$condition)), noms_condition(projet)$ids)
}

# Ce que le script d'un modèle à condition en écrit (script.R : code_observations ; les scripts des
# autres analyses), NULL sans condition : `texte`, la condition avec les noms des colonnes du tableau
# du script (`tab` : tableau_script, ses noms rendus uniques), un nom que la traduction du script en
# anglais changerait étant mis entre accents graves (elle les laisse) ; `cochees`, les rangs dans ce
# tableau des observations cochées ; `qualitatives`, si elle lit une variable qualitative (le script
# l'évalue alors sur les catégories en texte, comme Nuée) ; `fonctions`, celles des formules de
# Nouvelle variable qu'elle appelle (le script les écrit : code_fonctions_condition) ; `avertit`, si
# R avertit en l'évaluant (le script tait alors ces avertissements, comme Nuée).
plan_condition <- function(projet, d, tab) {
  if (!avec_condition(d)) return(NULL)
  texte <- texte_condition(d$condition)
  n <- noms_condition(projet)
  traduits <- c(names(NOMS_SCRIPT_EN), names(NOMS_CLASSEMENT_EN), names(NOMS_ORDINATION_EN))
  pd <- utils::getParseData(parse(text = texte, keep.source = TRUE, encoding = "UTF-8"))
  pd <- pd[pd$terminal & pd$token == "SYMBOL", , drop = FALSE]
  pd <- pd[order(-pd$line1, -pd$col1), , drop = FALSE]
  lignes <- strsplit(texte, "\n", fixed = TRUE)[[1]]
  qualitatives <- FALSE
  for (i in seq_len(nrow(pd))) {
    nom <- as.character(str2lang(pd$text[i]))
    colonne <- if (nom %in% names(n$variables)) {
      v <- variable_du_projet(projet, n$variables[[nom]])
      if (length(v$categories)) qualitatives <- TRUE
      unname(tab$noms[as.character(v$uid)])
    } else if (nom %in% n$ids) unname(tab$ids[[nom]])
    if (is.null(colonne)) next  # (T, F, pi)
    ecrit <- if (identical(make.names(colonne), colonne) && !colonne %in% traduits) colonne else paste0("`", gsub("`", "\\`", colonne, fixed = TRUE), "`")
    l <- lignes[pd$line1[i]]
    lignes[pd$line1[i]] <- paste0(substr(l, 1, pd$col1[i] - 1), ecrit, substr(l, pd$col2[i] + 1, nchar(l)))
  }
  list(texte = paste(lignes, collapse = "\n"), cochees = unname(tab$rangs[as.character(d$cochees %||% d$observations)]),
       qualitatives = qualitatives, fonctions = intersect(all.names(lire_condition(projet, texte)), FONCTIONS_NUEE_CONDITION),
       avertit = isTRUE(attr(valeurs_condition(projet, texte), "avertissement")), n_lignes = nrow(tab$donnees))
}

# Le code du script qui choisit les lignes d'un modèle à condition (`pc` : plan_condition) : les
# observations cochées, la condition évaluée sur toutes les lignes du tableau, et `lignes`, celles
# des cochées où elle est vraie (une valeur manquante comptant pour fausse), dans leur ordre.
# `quoi` : le commentaire qui les présente.
code_condition <- function(pc, tx, quoi) {
  donnees <- if (pc$qualitatives) "donnees_texte" else "donnees"
  appel <- sprintf("with(%s, %s)", donnees, pc$texte)
  if (pc$avertit) appel <- sprintf("suppressWarnings(%s)", appel)
  c(commentaire(c(quoi, tx("Les observations que Nuée prend (les cases cochées de Nouveau modèle : `cochees`, leurs rangs dans le tableau), puis celles où la condition du modèle est vraie (`lignes`) : elle est évaluée sur toutes les lignes du tableau, comme dans Nuée, et une valeur manquante compte pour fausse.",
                           "The observations Nuée takes (the boxes ticked in New model: `ticked`, their rows in the table), then those where the model's condition is TRUE (`rows`): it is evaluated on every row of the table, as in Nuée, and a missing value counts as FALSE."),
                  if (pc$qualitatives) tx("Les variables qualitatives y sont lues en texte, comme Nuée les lit.", "The qualitative variables are read as text there, as Nuée reads them."),
                  if (pc$avertit) tx("Les avertissements de R en l'évaluant sont tus, comme dans Nuée.", "R's warnings while evaluating it are silenced, as in Nuée."))),
    if (identical(as.integer(pc$cochees), seq_len(pc$n_lignes))) "cochees <- seq_len(nrow(donnees))" else code_valeurs(morceaux_rangs(pc$cochees), "cochees <- "),
    if (pc$qualitatives) "donnees_texte <- lapply(donnees, function(x) if (is.factor(x)) as.character(x) else x)",
    paste0("condition <- ", appel),
    "lignes <- cochees[condition[cochees] %in% TRUE]")
}

# Les fonctions des formules de Nouvelle variable qu'appelle une condition (`fonctions` : leurs
# noms, plan_condition), écrites pour le script comme variables.R les calcule. `dates` : écrire aussi
# temps_en_nombres() (script.R : modele_temps_dates), qui lit un temps fait de dates (une
# régression avec CAR1 le long de dates l'a déjà).
code_fonctions_condition <- function(fonctions, tx, dates = TRUE) {
  if (!length(fonctions)) return(character(0))
  par_groupe <- intersect(fonctions, c("moyenne_groupe", "group_mean", "ecart_groupe", "group_deviation"))
  decalees <- intersect(fonctions, c("precedent", "previous", "suivant", "following", "croissance", "growth_rate"))
  moyenne <- if ("moyenne_groupe" %in% par_groupe || !"group_mean" %in% par_groupe && "ecart_groupe" %in% par_groupe) "moyenne_groupe" else "group_mean"
  c(if (length(par_groupe)) c(
      commentaire(tx("Des fonctions des formules de Nouvelle variable que la condition du modèle appelle, comme Nuée les calcule. La moyenne de x dans le groupe de chaque observation (les valeurs manquantes ignorées ; une observation sans groupe n'en a pas), et l'écart de x à cette moyenne.",
                     "Functions of New variable formulas that the model's condition calls, as Nuée computes them. The mean of x in each observation's group (missing values ignored; an observation without a group has none), and the deviation of x from that mean.")),
      sprintf("%s <- function(x, groupe) {", moyenne),
      "  g <- as.character(groupe)",
      "  r <- ave(as.numeric(x), g, FUN = function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE))",
      "  r[is.na(g)] <- NA_real_",
      "  r",
      "}",
      if (all(c("moyenne_groupe", "group_mean") %in% par_groupe)) sprintf("%s <- %s", setdiff(c("moyenne_groupe", "group_mean"), moyenne), moyenne),
      vapply(intersect(c("ecart_groupe", "group_deviation"), par_groupe), function(f) sprintf("%s <- function(x, groupe) as.numeric(x) - %s(x, groupe)", f, moyenne), ""),
      ""),
    if (length(decalees)) c(
      if (dates) c(modele_temps_dates(tx), ""),
      commentaire(tx("Des fonctions des formules de Nouvelle variable que la condition du modèle appelle, comme Nuée les calcule. La valeur de x au temps « temps - pas » (precedent) ou « temps + pas » (suivant) dans le même groupe : manquante quand ce temps manque, jamais celle d'un temps plus ancien ; le taux de croissance, log(x / x du temps d'avant), manquant sauf si les deux sont positifs. Le temps : des nombres (l'année), ou des dates en années décimales.",
                     "Functions of New variable formulas that the model's condition calls, as Nuée computes them. The value of x at time \"time - step\" (previous) or \"time + step\" (following) in the same group: missing when that time is missing, never that of an older time; the growth rate, log(x / x of the time before), missing unless both are positive. The time: numbers (the year), or dates as decimal years.")),
      "lire_temps <- function(x) {",
      "  if (is.numeric(x)) return(ifelse(is.finite(x), x, NA))",
      "  x <- trimws(as.character(x))",
      "  n <- suppressWarnings(as.numeric(sub(\",\", \".\", x, fixed = TRUE)))",
      "  if (all(is.finite(n[!is.na(x) & nzchar(x)]))) return(ifelse(is.finite(n), n, NA))",
      tx("  temps_en_nombres(x)  # des dates", "  temps_en_nombres(x)  # dates"),
      "}",
      "valeur_decalee <- function(x, temps, groupe, pas, sens) {",
      "  t <- lire_temps(temps)",
      "  g <- if (is.null(groupe)) rep(\"\", length(t)) else as.character(groupe)",
      "  connu <- !is.na(t) & !is.na(g)",
      "  cle <- function(tt) ifelse(connu, paste(g, as.character(round(tt, 8)), sep = \"\\r\"), NA_character_)",
      "  x[match(cle(t + sens * pas), cle(t), incomparables = NA)]",
      "}",
      vapply(intersect(c("precedent", "previous"), decalees), function(f) sprintf("%s <- function(x, temps, groupe = NULL, pas = 1) valeur_decalee(x, temps, groupe, pas, -1)", f), ""),
      vapply(intersect(c("suivant", "following"), decalees), function(f) sprintf("%s <- function(x, temps, groupe = NULL, pas = 1) valeur_decalee(x, temps, groupe, pas, 1)", f), ""),
      unlist(lapply(intersect(c("croissance", "growth_rate"), decalees), function(f) c(
        sprintf("%s <- function(x, temps, groupe = NULL) {", f),
        "  avant <- valeur_decalee(x, temps, groupe, 1, -1)",
        "  r <- rep(NA_real_, length(x))",
        "  ok <- !is.na(x) & !is.na(avant) & x > 0 & avant > 0",
        "  r[ok] <- log(x[ok] / avant[ok])",
        "  r",
        "}"))),
      ""))
}
