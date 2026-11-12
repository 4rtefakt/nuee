# Analyse discriminante linéaire (Fisher, 1936 ; MASS::lda, Venables et Ripley, 2002) : les
# combinaisons linéaires des variables X qui séparent le mieux les classes d'une variable
# qualitative (rôle Classe), et le classement de chaque observation dans la classe de plus grande
# probabilité a posteriori. Les X entrent par leurs valeurs brutes (transformées s'il y a lieu ;
# une variable qualitative par les indicatrices de ses catégories, un terme croisé par le produit
# de ses facteurs), sans les termes constants ni ceux qui se déduisent des autres. Les
# probabilités a priori sont proportionnelles aux effectifs des classes, ou égales.
#
# Le calcul est fait sur les X divisés par leur écart-type (les scores, les probabilités et le
# classement n'en dépendent pas ; le seuil de MASS pour une variable constante dans les classes
# devient relatif) ; les coefficients sont rendus dans les unités des variables.
#
# Validation croisée par les groupes de la définition : chaque groupe est classé par le modèle
# ajusté sans lui (ses termes constants retirés, ses probabilités a priori recalculées).

# Les réglages d'une LDA (d$options).
reglages_lda <- function(d) list(priors = option_choix(d, "priors", c("proportionnelles", "egales")))

# Les termes X d'une analyse sur valeurs brutes : sans centrage, mise à l'échelle, bloc ni
# modificateur (construire_bloc en fait les valeurs transformées, et les termes croisés le
# produit de leurs facteurs).
termes_bruts <- function(termes) lapply(termes, function(t) { t$scaling <- list(type = "none"); t })

# Les colonnes de X qui peuvent entrer dans un modèle : sans les constantes ni celles qui se
# déduisent des autres (une catégorie quand toutes y sont), dans leur ordre.
colonnes_utiles <- function(X) {
  if (!ncol(X) || !nrow(X)) return(integer(0))
  q <- qr(cbind(1, X), tol = 1e-9)
  setdiff(sort(q$pivot[seq_len(q$rank)]), 1) - 1
}

# La LDA de MASS sur X divisé par ses écarts-types ; les probabilités a priori proportionnelles
# aux effectifs des classes ou égales. Une variable constante dans chaque classe est refusée
# par son nom (MASS ne la donne que par son rang).
lda_nuee <- function(X, classe, priors = "proportionnelles") {
  classe <- droplevels(classe)
  et <- apply(X, 2, stats::sd)
  Z <- sweep(X, 2, et, "/")
  moyennes <- rowsum(Z, classe) / as.vector(table(classe))
  intra <- sqrt(apply(Z - moyennes[as.integer(classe), , drop = FALSE], 2, stats::var))
  if (any(intra < 1e-4)) {
    stop(sprintf(traduire("Constante dans chaque classe, une variable ne se prête pas à la LDA (elle sépare les classes à elle seule, ou ne varie qu'entre elles) : %s. La retirer des X.",
                          "Constant within each class, a variable doesn't suit an LDA (it separates the classes on its own, or varies only between them): %s. Remove it from the X."),
                 paste(colnames(X)[intra < 1e-4], collapse = ", ")), call. = FALSE)
  }
  prior <- if (identical(priors, "egales")) rep(1 / nlevels(classe), nlevels(classe)) else as.vector(table(classe)) / length(classe)
  ajustement <- MASS::lda(Z, grouping = classe, prior = prior)
  ajustement$et <- et
  ajustement
}

# Les probabilités a posteriori (une colonne par classe de `classes`, 0 pour une classe que le
# modèle n'a pas vue) et les scores discriminants de lignes de X (dans ses colonnes).
predire_lda_matrice <- function(a, X, classes) {
  P <- matrix(NA_real_, nrow(X), length(classes), dimnames = list(NULL, classes))
  S <- matrix(NA_real_, nrow(X), ncol(a$scaling), dimnames = list(NULL, colnames(a$scaling)))
  ok <- stats::complete.cases(X)
  if (any(ok)) {
    p <- stats::predict(a, sweep(X[ok, , drop = FALSE], 2, a$et, "/"))
    P[ok, ] <- 0
    P[ok, colnames(p$posterior)] <- p$posterior
    S[ok, ] <- p$x
  }
  list(proba = P, scores = S)
}

# La validation croisée : chaque groupe classé par le modèle ajusté sans lui. Un groupe dont
# les autres n'ont qu'une classe, ou dont le modèle ne s'ajuste pas, reste sans prédiction.
validation_lda <- function(X, classe, groupes, priors) {
  classes <- levels(classe)
  P <- matrix(NA_real_, nrow(X), length(classes), dimnames = list(NULL, classes))
  echecs <- 0
  for (g in unique(groupes)) {
    k <- groupes == g
    ca <- droplevels(classe[!k])
    j <- colonnes_utiles(X[!k, , drop = FALSE])
    a <- if (nlevels(ca) >= 2 && length(j)) tryCatch(suppressWarnings(lda_nuee(X[!k, j, drop = FALSE], ca, priors)), error = function(e) NULL)
    if (is.null(a)) { echecs <- echecs + 1; next }
    P[k, ] <- predire_lda_matrice(a, X[k, j, drop = FALSE], classes)$proba
  }
  list(proba = P, echecs = echecs)
}

ajuster_lda <- function(projet, d) {
  r <- reglages_lda(d)
  if (!length(d$classe) || is.na(d$classe[1])) {
    stop(traduire("Une LDA demande une variable de classe (rôle Classe, une variable qualitative).", "An LDA needs a class variable (Class role, a qualitative variable)."), call. = FALSE)
  }
  if (length(d$y)) stop(traduire("Une LDA n'a pas de variable Y : sa réponse est la classe.", "An LDA has no Y variable: its response is the class."), call. = FALSE)
  if (!length(d$x)) stop(traduire("Une LDA demande au moins une variable X.", "An LDA needs at least one X variable."), call. = FALSE)
  termes <- termes_bruts(d$x)
  obs <- d$observations
  classe <- classe_observations(projet, d$classe, obs)
  bx <- construire_bloc(projet, termes, obs)
  complet <- !is.na(classe) & stats::complete.cases(bx$X)
  if (sum(complet) < 4) stop(traduire("Trop peu d'observations avec une classe et toutes leurs valeurs X.", "Too few observations with a class and all their X values."), call. = FALSE)
  garde <- obs[complet]
  cl <- droplevels(classe[complet])
  if (nlevels(cl) < 2) stop(traduire("Une LDA demande au moins deux classes.", "An LDA needs at least two classes."), call. = FALSE)
  X <- bx$X[complet, , drop = FALSE]
  j <- colonnes_utiles(X)
  if (!length(j)) stop(traduire("Aucune variable X ne varie sur ces observations.", "No X variable varies over these observations."), call. = FALSE)
  retires <- colnames(X)[-j]
  X <- X[, j, drop = FALSE]
  if (nrow(X) - nlevels(cl) < ncol(X)) {
    stop(sprintf(traduire("Trop de termes X (%d) pour %d observations en %d classes : il en faut au moins autant que de termes plus de classes.",
                          "Too many X terms (%d) for %d observations in %d classes: there must be at least as many as terms plus classes."),
                 ncol(X), nrow(X), nlevels(cl)), call. = FALSE)
  }
  colineaires <- FALSE
  a <- withCallingHandlers(lda_nuee(X, cl, r$priors), warning = function(w) {
    # (le message de MASS, dans la langue de R : « variables are collinear », « les variables sont colinéaires »...)
    if (grepl(gettext("variables are collinear", domain = "R-MASS"), conditionMessage(w), fixed = TRUE) || grepl("collinear", conditionMessage(w))) {
      colineaires <<- TRUE
      invokeRestart("muffleWarning")
    }
  })
  classes <- levels(cl)
  groupes <- groupes_observations(projet, d, garde)
  ajuste <- predire_lda_matrice(a, X, classes)
  vc <- validation_lda(X, cl, groupes, r$priors)
  scores <- ajuste$scores
  axes <- colnames(a$scaling)
  m <- list(type = "LDA", N = nrow(X), K = ncol(X), observations = garde, exclues = setdiff(obs, garde), classe_nom = as.character(variable_du_projet(projet, d$classe)$ids[1]),
            classes = classes, effectifs = as.vector(table(cl)), reglages_lda = r, termes = termes, colonnes = colnames(X), retires = retires,
            donnees = X, classe = cl, groupes = groupes, G = length(unique(groupes)), vc_par = nom_groupes_vc(projet, d), ajustement = a,
            colineaires = colineaires, proba = ajuste$proba, scores = scores, axes = axes, vc = vc,
            classification = classification_plsda(indicatrices(cl, classes), ajuste$proba, vc$proba, classes),
            # les coefficients dans les unités des variables, et pour des variables d'écart-type 1
            coefficients = a$scaling / a$et, coefficients_std = unclass(a$scaling),
            structure = stats::cor(X, scores), part = a$svd^2 / sum(a$svd^2),
            moyennes = rowsum(X, cl) / as.vector(table(cl)), centroides = rowsum(scores, cl) / as.vector(table(cl)))
  m
}

# Les X d'observations, dans les colonnes du modèle.
x_lda <- function(projet, m, observations) construire_bloc(projet, m$termes, observations)$X[, m$colonnes, drop = FALSE]

# Le jeu de test d'une LDA : ses observations classées par le modèle (une observation
# incomplète reste sans classe) ; ajuster_analyse (analyses.R) l'appelle après l'ajustement.
tester_lda <- function(projet, m, d) {
  test <- as.numeric(d$test)
  y <- as.character(classe_observations(projet, d$classe, test))
  p <- classe_la_plus_probable(predire_lda_matrice(m$ajustement, x_lda(projet, m, test), m$classes)$proba, m$classes)
  obs <- data.frame(uid = test, nom = unname(projet$noms_obs[as.character(test)]), hors_domaine = FALSE, classe = y, classe_predite = p, stringsAsFactors = FALSE)
  list(n = length(test), hors_domaine = 0, observations = obs, classement = classement_test(y, p, m$classes))
}

# Les observations d'une LDA : classe observée, ajustée et prédite en validation croisée,
# scores discriminants, probabilités a posteriori, groupe de validation croisée.
observations_lda <- function(projet, m) {
  obs <- data.frame(uid = m$observations, nom = unname(projet$noms_obs[as.character(m$observations)]), classe = as.character(m$classe),
                    stringsAsFactors = FALSE)
  obs[["classe ajustée"]] <- m$classification$ajuste$predite
  obs[["classe prédite (VC)"]] <- m$classification$vc$predite
  for (a in m$axes) obs[[a]] <- unname(m$scores[, a])
  for (k in m$classes) obs[[paste("probabilité", k)]] <- unname(m$proba[, k])
  obs$groupe_vc <- m$groupes
  obs
}

# Une matrice de résultats en tableau : son nom de ligne (`cle`), puis ses colonnes.
tableau_lignes <- function(M, cle) {
  out <- data.frame(rownames(M), unclass(M), check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
  names(out)[1] <- cle
  out
}

resultat_lda <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  r <- m$classification
  list(id = id, projet = p$id, nom = d$name %||% "", titre = d$title %||% "", type = "LDA", analyse = TRUE, prediction = TRUE, regression = FALSE,
       opls = FALSE, Ap = 0, A = 0, N = m$N, K = m$K, noms_y = I(m$classe_nom), classes = I(m$classes), observations = observations_lda(p, m),
       axes = I(m$axes), part = I(unname(m$part)),
       coefficients = tableau_lignes(m$coefficients, "terme"), coefficients_std = tableau_lignes(m$coefficients_std, "terme"),
       structure = tableau_lignes(m$structure, "terme"), moyennes = tableau_lignes(m$moyennes, "classe"),
       centroides = tableau_lignes(m$centroides, "classe"),
       priors = data.frame(classe = m$classes, n = m$effectifs, prior = unname(m$ajustement$prior), stringsAsFactors = FALSE),
       reglages_lda = m$reglages_lda, classification = r, qualite = list(bien_classes = r$ajuste$global, bien_classes_vc = r$vc$global),
       termes_retires = I(m$retires), colineaires = m$colineaires, echecs_vc = m$vc$echecs, exclues_manquantes = length(m$exclues),
       G = m$G, vc_par = m$vc_par %||% "", reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app),
       note = d$note %||% "", test = m$test, demande = d$demande %||% list(type = d$type, numero = d$number),
       vues_defaut = I(c("lda_scores", "classement")),
       chiffres = sprintf("%s %s · %s %s", fmt_pourcent(r$vc$global), traduire("bien classées (VC)", "correct (CV)"), m$axes[1], fmt_pourcent(m$part[1])))
}

predire_lda <- function(projet, m, d, observations) {
  r <- predire_lda_matrice(m$ajustement, x_lda(projet, m, observations), m$classes)
  out <- data.frame(row.names = seq_along(observations))
  out[["classe prédite"]] <- classe_la_plus_probable(r$proba, m$classes)
  for (k in m$classes) out[[paste("probabilité", k)]] <- unname(r$proba[, k])
  for (a in m$axes) out[[a]] <- unname(r$scores[, a])
  if (length(projet$variables)) out[["classe observée"]] <- as.character(classe_observations(projet, d$classe, observations))
  out$hors_domaine <- FALSE
  out
}

tables_lda <- function(projet, m) {
  obs <- observations_lda(projet, m)
  obs$uid <- NULL
  names(obs)[names(obs) == "nom"] <- "Observation"
  r <- m$classification
  list(resume = data.frame(N = m$N, termes = m$K, classes = length(m$classes), groupes_vc = m$G, priors = m$reglages_lda$priors,
                           bien_classees = r$ajuste$global, bien_classees_vc = r$vc$global, stringsAsFactors = FALSE),
       observations = obs, coefficients = tableau_lignes(m$coefficients, "terme"), coefficients_std = tableau_lignes(m$coefficients_std, "terme"),
       structure = tableau_lignes(m$structure, "terme"),
       part = data.frame(axe = m$axes, part = unname(m$part), stringsAsFactors = FALSE),
       moyennes = tableau_lignes(m$moyennes, "classe"),
       priors = data.frame(classe = m$classes, n = m$effectifs, prior = unname(m$ajustement$prior), stringsAsFactors = FALSE),
       classement = tableau_classes(r$ajuste$table, m$classes), classement_vc = tableau_classes(r$vc$table, m$classes))
}

texte_methodes_lda <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  g <- function(x) guillemets_texte(x, fr)
  r <- m$classification
  effectifs <- sprintf("%s (%d)", m$classes, m$effectifs)
  p1 <- sprintf(if (fr) "L'analyse porte sur %d observations%s, réparties en %d classes de %s : %s."
                else "The analysis uses %d observations%s, in %d classes of %s: %s.",
                m$N, if (length(m$exclues)) sprintf(if (fr) " (%d autres laissées de côté : leur classe ou une valeur X manque)" else " (%d others left out: their class or an X value is missing)", length(m$exclues)) else "",
                length(m$classes), g(m$classe_nom), liste_mots(effectifs, fr))
  noms <- m$colonnes
  p2 <- sprintf(if (fr) "Une analyse discriminante linéaire (Fisher, 1936) a été ajustée avec la fonction lda du paquet MASS (Venables & Ripley, 2002) sur %s, par leurs valeurs brutes%s ; les probabilités a priori des classes sont %s. Chaque observation est rangée dans la classe de plus grande probabilité a posteriori."
                else "A linear discriminant analysis (Fisher, 1936) was fitted with the lda function of the MASS package (Venables & Ripley, 2002) on %s, by their raw values%s; the prior probabilities of the classes are %s. Each observation is assigned to the class of largest posterior probability.",
                if (length(noms) <= 12) sprintf(if (fr) "%d termes X (%s)" else "%d X terms (%s)", length(noms), liste_mots(noms, fr)) else sprintf(if (fr) "%d termes X" else "%d X terms", length(noms)),
                if (length(m$retires)) sprintf(if (fr) " (%s, constants ou déduits des autres, laissés de côté)" else " (%s, constant or implied by the others, left out)", liste_mots(m$retires, fr)) else "",
                if (m$reglages_lda$priors == "egales") (if (fr) "égales" else "equal") else (if (fr) "proportionnelles à leurs effectifs" else "proportional to their sizes"))
  p3 <- sprintf(if (fr) "Le classement a été validé par une validation croisée %s, chaque groupe étant classé par le modèle ajusté sans lui%s."
                else "The classification was checked by a cross-validation %s, each group being classified by the model fitted without it%s.",
                groupes_texte(m$vc_par, m$G, fr),
                if (m$vc$echecs) sprintf(if (fr) " (%d groupes sans prédiction : sans eux, une seule classe reste, ou le modèle ne s'ajuste pas)" else " (%d groups without a prediction: without them, a single class remains, or the model doesn't fit)", m$vc$echecs) else "")
  p4 <- sprintf(if (fr) "La première fonction discriminante porte %s de la séparation entre classes%s ; %s des observations sont bien classées, %s en validation croisée."
                else "The first discriminant function carries %s of the separation between classes%s; %s of the observations are correctly classified, %s in cross-validation.",
                pourcent_texte(m$part[1], fr),
                if (length(m$part) > 1) sprintf(if (fr) ", la deuxième %s" else ", the second %s", pourcent_texte(m$part[2], fr)) else "",
                pourcent_texte(r$ajuste$global, fr), pourcent_texte(r$vc$global, fr))
  if (!is.null(m$test)) {
    p4 <- paste(p4, sprintf(if (fr) "Un jeu de test de %d observations, laissées hors du modèle et de la validation croisée, a ensuite été classé : %s bien classées%s."
                            else "A test set of %d observations, left out of the model and of the cross-validation, was then classified: %s correctly classified%s.",
                            m$test$n, pourcent_texte(m$test$classement$bien_classees, fr), classes_inconnues_texte(m$test$classement$inconnues, fr)))
  }
  logiciels <- references_classement(c("fisher", "venables"), "MASS", fr, version)
  list(paragraphes = c(p1, p2, p3, p4, logiciels$phrase), references = logiciels$references)
}
