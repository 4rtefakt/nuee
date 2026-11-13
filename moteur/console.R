# Console R de l'application (Affichage › Console R), lue par serveur.R :
#   - le journal : pour chaque action de l'interface, l'appel au moteur qu'elle a fait, écrit
#     en R (pour refaire le calcul ou le recopier dans un script) ;
#   - l'évaluation de commandes R tapées dans la console, dans la session R de l'application.
# Dans la console, P1, M7... sont les projets et les modèles ouverts (les noms qu'ils ont dans
# le journal), `p` et `m` le projet et le modèle affichés. Modifier un projet
# (P1$donnees[...] <- ...) le modifie dans l'application.

etat$journal <- list()
etat$n_journal <- 0
etat$console_modifies <- character(0)

# Une entrée du journal : type "appel" (action de l'interface), "commande", "sortie",
# "avertissement", "erreur" ou "image" (graphique PNG en base64).
noter <- function(type, texte) {
  etat$n_journal <- etat$n_journal + 1
  etat$journal[[length(etat$journal) + 1]] <- list(n = etat$n_journal, type = type, heure = format(Sys.time(), "%H:%M:%S"),
                                                   texte = enc2utf8(paste(texte, collapse = "\n")))
  if (length(etat$journal) > 2000) etat$journal <- utils::tail(etat$journal, 1500)
  invisible(NULL)
}

journal_depuis <- function(n) Filter(function(e) e$n > n, etat$journal)

# ---------------------------------------------------------------- le journal des actions

code_r <- function(x) deparse(x, width.cutoff = 500L)
vecteur_r <- function(x) if (length(x) == 1) code_r(x) else paste0("c(", paste(vapply(x, code_r, ""), collapse = ", "), ")")

appel_ouverture <- function(chemin, table = NULL) {
  ext <- tolower(tools::file_ext(chemin))
  if (ext == "usp") return(sprintf("lire_projet(convertir_usp(%s))  # .usp converti par export_project", code_r(chemin)))
  if (ext %in% c("nuee", "json")) return(sprintf("lire_projet(%s)", code_r(chemin)))
  if (!is.null(table)) return(sprintf("importer_tableau(%s, table = %s)", code_r(chemin), code_r(table)))
  sprintf("importer_tableau(%s)", code_r(chemin))
}

# La lecture du tableau d'Ajouter des colonnes d'un tableau (serveur.R : tableau_a_joindre).
appel_tableau <- function(chemin, table = NULL, textes = NULL) {
  ext <- tolower(tools::file_ext(chemin))
  if (ext == "usp") return(sprintf("tableau_projet(lire_projet(convertir_usp(%s)))", code_r(chemin)))
  if (ext %in% c("nuee", "json")) return(sprintf("tableau_projet(lire_projet(%s))", code_r(chemin)))
  sprintf("lire_tableau(%s%s%s)", code_r(chemin), if (!is.null(table)) sprintf(", table = %s", code_r(table)) else "",
          if (length(textes)) sprintf(", textes = %s", vecteur_r(textes)) else "")
}

# Des colonnes du projet (des numéros de variables, ou "id:k") en R : 4, "id:2", c("id:2", 4).
colonnes_projet_r <- function(x) {
  x <- as.character(unlist(x))
  e <- ifelse(startsWith(x, "id:"), vapply(x, code_r, ""), x)
  if (length(e) == 1) e else paste0("c(", paste(e, collapse = ", "), ")")
}

# Une colonne d'un data.frame en R : d$site, ou d[["habitat (2)"]] pour un nom qui n'est pas un nom de R.
colonne_r <- function(df, nom) if (identical(make.names(nom), nom)) sprintf("%s$%s", df, nom) else sprintf("%s[[%s]]", df, code_r(nom))

# Les lignes R d'une action de l'interface (NULL pour celles qui ne font qu'afficher).
journaliser <- function(nom, q, r) {
  pm <- function(id) etat$modeles[[id]]$projet
  variable_r <- function(x) if (is.numeric(x)) as.character(x) else code_r(x)  # (un numéro de variable, ou "id:k")
  definition_modele <- function(id, commentaire) {
    e <- etat$modeles[[id]]
    c(commentaire, code_definition(projet(e$projet), e$definition, e$projet), sprintf("%s <- ajuster(%s, d)", id, e$projet))
  }
  lignes <- switch(nom,
    ouvrir = if (!is.null(r$id)) sprintf("%s <- %s", r$id, appel_ouverture(q$chemin, q$table)),
    coller = c("# tableau collé, gardé dans un fichier de la session", sprintf("%s <- importer_tableau(%s)", r$id, code_r(r$fichier_colle))),
    importer_spectres = sprintf("%s <- importer_spectres(%s%s, nom = %s)", r$id, vecteur_r(as.character(unlist(q$chemins))),
                                if (length(q$infos) && nzchar(q$infos)) sprintf(", infos = %s", code_r(q$infos)) else "", code_r(r$nom)),
    exporter = sprintf("ecrire_tables(tables_resultats(%s, %s), %s)", pm(q$modele), q$modele, code_r(r$chemin)),
    exporter_donnees = sprintf("ecrire_tableau(tableau_projet(%s), %s)", q$projet, code_r(r$chemin)),
    fermer_projet = sprintf("rm(%s)  # projet fermé", q$projet),
    ajuster = definition_modele(r$id, sprintf("# modèle %s (%s) du projet %s", r$nom, r$type, projet(r$projet)$nom)),
    reajuster = definition_modele(r$id, sprintf("# modèle %s, d'après %s%s", r$nom, q$modele,
      if (length(q$exclues)) sprintf(", sans %d observation(s)", length(q$exclues)) else if (length(q$garder)) ", variables VIP"
      else if (!is.null(q$groupes_aic)) ", choisi par la sélection par AIC" else if (!is.null(q$car1)) ", avec une autocorrélation CAR1" else "")),
    baci_modele = c(if (length(r$nouvelles)) sprintf("%s <- variables_baci(%s, %s, %s, %s, %s, %s)$projet  # variables 0/1 de la période et de l'impact",
                                                     q$projet, q$projet, variable_r(q$temps), variable_r(q$unite), vecteur_r(as.character(unlist(q$impact))),
                                                     vecteur_r(as.character(unlist(q$temoins))), code_r(q$evenement)),
                    definition_modele(r$modele$id, sprintf("# modèle mixte BACI %s du projet %s", r$modele$nom, projet(r$modele$projet)$nom))),
    selection_aic = sprintf("selection_aic(%s, definition(%s))", q$modele, code_r(q$modele)),
    predire = sprintf("predire(%s, %s, definition(%s), %s)", pm(q$modele), q$modele, code_r(q$modele),
                      if (is.null(q$jeu)) sprintf("numeros_observations(%s)", pm(q$modele)) else sprintf("%s$prediction_sets[[%s]]$observations", pm(q$modele), q$jeu)),
    predire_fichier = sprintf("predire_fichier(%s, %s, definition(%s), %s)", pm(q$modele), q$modele, code_r(q$modele), code_r(q$chemin)),
    hca = sprintf("classification(%s, %s)", q$modele, q$composantes %||% sprintf("%s$A", q$modele)),
    permutations = sprintf("permutations(%s, %s, definition(%s), y = %s, n = %s)", pm(q$modele), q$modele, code_r(q$modele), q$y %||% 1, q$n %||% 20),
    contributions = sprintf("contributions(%s, %s, %s)", q$modele, vecteur_r(as.numeric(q$observations)), code_r(q$type %||% "DModX")),
    biplot = sprintf("correlations_biplot(%s)", q$modele),
    ipls = sprintf("pls_par_intervalles(%s, definition(%s), n = %d, A = %d)", pm(q$modele), code_r(q$modele), length(r$intervalles), as.integer(r$A)),
    scores_en_variables = sprintf("%s <- scores_en_variables(%s, %s)$projet", pm(q$modele), pm(q$modele), q$modele),
    classe_selection = sprintf("%s <- ajouter_classe(%s, %s, %s, %s)", q$projet, q$projet, code_r(q$variable), code_r(q$etiquette), vecteur_r(as.numeric(q$observations))),
    variable_calculee = sprintf("%s <- variable_calculee(%s, %s, %s)", q$projet, q$projet, code_r(q$nom), code_r(q$formule)),
    classes_valeurs = sprintf("%s <- classes_valeurs(%s, %s, %s, %s)", q$projet, q$projet, code_r(q$nom), q$variable,
                              if (length(q$bornes)) sprintf("bornes = %s", vecteur_r(q$bornes)) else sprintf("n = %s", q$n)),
    # (et, en commentaire, le même calcul fait avec match() sur le tableau des données)
    ajouter_colonnes = {
      p <- projet(q$projet)
      cles <- as.character(unlist(q$cles_table))
      parties <- lapply(seq_along(cles), function(j) {
        v <- colonne_ou_identifiant(p, as.character(unlist(q$cles_projet))[j], numeros_observations(p)[1])
        texte <- function(x) if (v$nombres) x else sprintf("trimws(%s)", x)
        c(texte(colonne_r("d", v$nom)), texte(colonne_r("tableau", cles[j])))
      })
      cle <- function(k) if (length(parties) == 1) parties[[1]][k] else sprintf("paste(%s)", paste(vapply(parties, `[`, "", k), collapse = ", "))
      c(sprintf("tableau <- %s%s", appel_tableau(q$chemin, q$table, cles), if (isTRUE(q$colle)) "  # cellules collées, gardées dans un fichier de la session" else ""),
        sprintf("%s <- ajouter_colonnes_table(%s, tableau, cles_projet = %s, cles_table = %s, colonnes = %s)", q$projet, q$projet,
                colonnes_projet_r(q$cles_projet), vecteur_r(cles), vecteur_r(r$jointure$colonnes)),
        sprintf("# soit, sur le tableau des données : d <- tableau_projet(%s) ; k <- match(%s, %s) ; d[%s] <- tableau[k, %s]", q$projet,
                cle(1), cle(2), vecteur_r(r$jointure$noms), vecteur_r(r$jointure$colonnes)))
    },
    resumer_groupes = {
      p <- projet(q$projet)
      groupes <- as.character(unlist(q$groupes))
      noms_groupes <- vapply(groupes, function(x) colonne_ou_identifiant(p, x, numeros_observations(p)[1])$nom, "", USE.NAMES = FALSE)
      variables <- as.numeric(unlist(q$variables))
      stats <- as.character(unlist(q$statistiques %||% "moyenne"))
      en_r <- c(moyenne = "mean(x, na.rm = TRUE)", somme = "sum(x, na.rm = TRUE)", mediane = "median(x, na.rm = TRUE)", minimum = "min(x, na.rm = TRUE)",
                maximum = "max(x, na.rm = TRUE)", ecart_type = "sd(x, na.rm = TRUE)", n = "sum(!is.na(x))")
      c(sprintf("%s <- resumer_par_groupe(%s, %s, variables = %s, statistiques = %s)", r$id, q$projet, colonnes_projet_r(groupes), vecteur_r(variables), vecteur_r(stats)),
        sprintf("# soit, sur le tableau des données : d <- tableau_projet(%s) ; aggregate(d[%s], d[%s], function(x) c(%s))", q$projet,
                vecteur_r(unname(p$noms_var[as.character(variables)])), vecteur_r(noms_groupes), paste(sprintf("%s = %s", stats, en_r[stats]), collapse = ", ")))
    },
    modifier_valeur = sprintf("%s <- modifier_valeur(%s, %s, %s, %s)", q$projet, q$projet, q$observation, q$variable, code_r(q$valeur %||% "")),
    statistiques_variables = sprintf("statistiques_variables(%s)", q$projet),
    series = {
      # (avec les tendances, leur appel sur les points, et les saisons du test saisonnier)
      appel <- sprintf("series_variable(%s)", paste(c(q$projet, variable_r(q$variable), variable_r(q$temps),
        if (length(q$lignes)) paste("lignes =", variable_r(q$lignes)), if (length(q$panneaux)) paste("panneaux =", variable_r(q$panneaux)),
        if (isTRUE(q$tendances) && length(q$saisons) && nzchar(as.character(q$saisons))) paste("saisons =", variable_r(q$saisons))), collapse = ", "))
      if (isTRUE(q$tendances)) sprintf("tendances_series(%s$points, %s%s)", appel, code_r(q$correction %||% "BH"),
                                       if (isTRUE(q$autocorrelation)) ", autocorrelation = TRUE" else "") else appel
    },
    historique = sprintf("historique_projet(%s)", q$projet),
    enregistrer = sprintf("enregistrer_projet(%s, %s)", q$projet, code_r(r$chemin)),
    script_r = if (length(q$modeles) > 1) {
      ids <- as.character(unlist(q$modeles))
      sprintf("ecrire_script_seance(%s, lapply(%s, definition), list(%s), %s, dossier_moteur(), donnees = %s%s)", pm(ids[1]), code_r(ids),
              paste(ids, collapse = ", "), code_r(r$script), code_r(if (identical(q$donnees, "csv")) "csv" else "script"),
              paste0(vapply(c("renv", "rdata", "rproj"), function(o) if (isTRUE(q[[o]])) sprintf(", %s = TRUE", o) else "", ""), collapse = ""))
    } else sprintf("ecrire_script(%s, definition(%s), %s, dossier_moteur(), m = %s, donnees = %s%s)", pm(q$modele), code_r(q$modele),
                   code_r(r$script), q$modele, code_r(if (identical(q$donnees, "csv")) "csv" else "script"),
                   paste0(vapply(c("renv", "rdata", "rproj"), function(o) if (isTRUE(q[[o]])) sprintf(", %s = TRUE", o) else "", ""), collapse = "")),
    NULL)
  if (length(lignes)) noter("appel", lignes)
  # un modèle ajusté : ce que R en affiche, comme après l'avoir tapé dans la console
  if (nom %in% c("ajuster", "reajuster", "baci_modele")) {
    id <- if (nom == "baci_modele") r$modele$id else r$id
    if (!is.null(etat$modeles[[id]])) noter("resultat", utils::capture.output(print(etat$modeles[[id]]$modele)))
  }
}

# ---------------------------------------------------------------- la console

console <- new.env(parent = globalenv())

# Les projets et les modèles ouverts, liés à l'état de l'application. Un projet se remplace
# par un projet (P1 <- variable_calculee(P1, ...)) ; un modèle se refait avec ajuster().
lier_objets <- function() {
  for (k in ls(console)) {
    if (grepl("^[PM]\\d+$", k) && bindingIsActive(k, console) && is.null(etat$projets[[k]]) && is.null(etat$modeles[[k]])) rm(list = k, envir = console)
  }
  for (id in names(etat$projets)) if (!exists(id, envir = console, inherits = FALSE)) local({
    k <- id
    makeActiveBinding(k, function(v) {
      if (missing(v)) return(etat$projets[[k]])
      if (!inherits(v, "projet_nuee")) stop(sprintf(traduire("%s est un projet de l'application : lui donner un autre nom", "%s is one of the application's projects: give it another name"), k))
      memoriser(etat$projets[[k]], "changement depuis la console")
      v$id <- k
      v$modifie <- TRUE
      etat$projets[[k]] <- v
      reviser(k)
      etat$console_modifies <- unique(c(etat$console_modifies, k))
    }, console)
  })
  for (id in names(etat$modeles)) if (!exists(id, envir = console, inherits = FALSE)) local({
    k <- id
    makeActiveBinding(k, function(v) {
      if (missing(v)) return(etat$modeles[[k]]$modele)
      stop(sprintf(traduire("%s est un modèle de l'application : donner un autre nom au résultat", "%s is one of the application's models: give the result another name"), k))
    }, console)
  })
}

# Outils de la console, présentés par aide_console().
local({
  assign("definition", function(id) (etat$modeles[[id]] %||% stop(traduire("Modèle inconnu : ", "Unknown model: "), id))$definition, console)
  assign("projets", function() data.frame(objet = names(etat$projets),
    nom = vapply(etat$projets, function(p) p$nom %||% "", ""), row.names = NULL), console)
  assign("modeles", function() data.frame(objet = names(etat$modeles),
    nom = vapply(etat$modeles, function(e) e$definition$name %||% "", ""),
    type = vapply(etat$modeles, function(e) paste0(if (isTRUE(e$modele$opls)) "O" else "", if (e$modele$plsda) "PLS-DA" else e$modele$type), ""),
    A = vapply(etat$modeles, function(e) as.numeric(e$modele$A), 0), projet = vapply(etat$modeles, function(e) e$projet, ""),
    row.names = NULL), console)
  assign("dossier_moteur", function() normalizePath(file.path(racine, "moteur", "R"), winslash = "/"), console)
  assign("recharger_moteur", function() {
    for (f in sort(list.files(file.path(racine, "moteur", "R"), "\\.R$", full.names = TRUE))) sys.source(f, envir = globalenv(), keep.source = FALSE)
    message("Moteur rechargé depuis ", normalizePath(file.path(racine, "moteur", "R"), winslash = "/"), " : réajuster les modèles pour l'utiliser.")
  }, console)
  # Recalcule les modèles enregistrés des projets ouverts (ou des fichiers .usp / .json d'un
  # dossier) et les compare aux valeurs du fichier (voir comparer_references, moteur/R/verification.R).
  assign("verifier_moteur", function(dossier = NULL) {
    projets <- if (is.null(dossier)) etat$projets else {
      fichiers <- list.files(dossier, "\\.(usp|json)$", full.names = TRUE, ignore.case = TRUE)
      stats::setNames(lapply(fichiers, function(f) tryCatch(
        if (grepl("\\.usp$", f, ignore.case = TRUE)) lire_projet(convertir_usp(f)) else lire_projet(f),
        error = function(e) NULL)), basename(fichiers))
    }
    tab <- do.call(rbind, lapply(names(projets), function(k) {
      p <- projets[[k]]
      if (is.null(p)) return(NULL)
      r <- comparer_references(p)
      if (nrow(r)) cbind(projet = p$nom %||% k, r)
    }))
    if (is.null(tab)) { message("Aucun modèle enregistré à comparer."); return(invisible(NULL)) }
    n <- table(factor(sub(" :.*", "", tab$statut), c("identique", "proche", "différent", "erreur")))
    message(sprintf("%d modèle(s) : %d identique(s), %d proche(s), %d différent(s), %d en erreur (Q2 non compté : la validation croisée de Nuée n'est pas celle du fichier).",
                    nrow(tab), n[["identique"]], n[["proche"]], n[["différent"]], n[["erreur"]]))
    tab
  }, console)
  quitter <- function(...) message("Pour fermer l'application : Fichier › Quitter.")
  assign("q", quitter, console)
  assign("quit", quitter, console)
})

# m et p : le modèle et le projet affichés dans l'application.
lier_m_p <- function(modele = NULL, projet = NULL) {
  lier_objets()
  if (!is.null(modele) && !is.null(etat$modeles[[modele]])) {
    assign("m", etat$modeles[[modele]]$modele, envir = console)
    projet <- etat$modeles[[modele]]$projet
  }
  if (exists("p", envir = console, inherits = FALSE)) rm("p", envir = console)
  if (!is.null(projet) && !is.null(etat$projets[[projet]])) makeActiveBinding("p", local({
    k <- projet
    function(v) if (missing(v)) get(k, envir = console) else assign(k, v, envir = console)
  }), console)
}

# Complétion (touche Tab) : les noms qui peuvent finir le mot tapé avant le curseur. Après
# « x$ » ou « x[[ », les noms de l'objet ; dans les parenthèses d'une fonction, ses arguments ;
# sinon les objets de la console et du moteur, et les fonctions de R. Rien n'est évalué : un
# objet n'est lu que par son nom suivi de $nom ou [["nom"]], jamais par un appel de fonction
# (la complétion de R le ferait, et pourrait relancer un calcul).
completer_console <- function(texte, curseur = nchar(texte), modele = NULL, projet = NULL) {
  lier_m_p(modele, projet)
  avant <- substr(texte, 1, curseur)
  mot <- regmatches(avant, regexpr("[[:alnum:]._]*$", avant))
  reste <- substr(avant, 1, nchar(avant) - nchar(mot))
  debut <- nchar(reste)
  sur <- function(n) ifelse(make.names(n) == n | !nzchar(n), n, paste0("`", gsub("`", "\\\\`", n), "`"))
  candidats <- character(0)
  chaine <- regmatches(reste, regexpr("[[:alpha:].][[:alnum:]._]*((\\$[[:alnum:]._]+)|(\\[\\[\"[^\"]*\"\\]\\]))*(\\$|\\[\\[\")$", reste))
  if (length(chaine)) {  # x$..., x$y$..., x[["...
    guillemet <- endsWith(chaine, "[[\"")
    morceaux <- regmatches(chaine, gregexpr("^[[:alpha:].][[:alnum:]._]*|\\$[[:alnum:]._]+|\\[\\[\"[^\"]*\"\\]\\]", chaine))[[1]]
    # (au-delà du premier nom, seulement les éléments de listes, sans leur classe : aucune
    # méthode `[[` ni liaison active d'un environnement n'est appelée)
    objet <- tryCatch({
      o <- get(morceaux[1], envir = console)
      for (k in morceaux[-1]) o <- if (is.list(o) && !isS4(o)) unclass(o)[[gsub("^\\$|^\\[\\[\"|\"\\]\\]$", "", k)]]
      o
    }, error = function(e) NULL)
    noms <- as.character(if (is.environment(objet)) ls(objet) else if (!isS4(objet)) names(unclass(objet)))  # (aucun : objet inconnu)
    candidats <- noms[startsWith(noms, mot)]
    if (!guillemet) candidats <- sur(candidats)
  } else {
    # l'appel de fonction dont la parenthèse est ouverte : ses arguments d'abord
    profondeur <- 0
    fonction <- NULL
    caracteres <- strsplit(reste, "")[[1]]
    for (i in rev(seq_along(caracteres))) {
      if (caracteres[i] == ")") profondeur <- profondeur + 1
      if (caracteres[i] == "(") {
        if (profondeur == 0) {
          fonction <- regmatches(substr(reste, 1, i - 1), regexpr("[[:alpha:].][[:alnum:]._]*\\s*$", substr(reste, 1, i - 1)))
          break
        }
        profondeur <- profondeur - 1
      }
    }
    arguments <- character(0)
    if (length(fonction) && nzchar(trimws(fonction))) arguments <- tryCatch({
      f <- get(trimws(fonction), envir = console, mode = "function")
      setdiff(names(formals(if (is.primitive(f)) args(f) else f)), "...")
    }, error = function(e) character(0))
    objets <- c(ls(console), ls(globalenv()), if (nzchar(mot)) utils::apropos(paste0("^", gsub("\\.", "\\\\.", mot))),
                "TRUE", "FALSE", "NULL", "NA", "function")
    candidats <- c(sprintf("%s = ", arguments[startsWith(arguments, mot)]), sort(unique(objets[startsWith(objets, mot)])))
  }
  list(debut = debut, mot = mot, candidats = I(utils::head(unique(candidats), 300)))
}

# Évalue une commande R. `modele` et `projet` : ceux affichés dans l'application, appelés
# m et p dans la commande. Le texte affiché, les avertissements, les erreurs et les
# graphiques vont au journal ; renvoie les entrées nouvelles et les projets modifiés.
evaluer_console <- function(texte, modele = NULL, projet = NULL) {
  depart <- etat$n_journal
  lier_m_p(modele, projet)
  noter("commande", texte)
  exprs <- tryCatch(parse(text = texte, keep.source = FALSE), error = function(e) e)
  if (inherits(exprs, "error")) {
    noter("erreur", conditionMessage(exprs))
  } else {
    motif <- file.path(tempdir(), sprintf("console-%d-%%03d.png", etat$n_journal))
    grDevices::png(motif, width = 900, height = 600, res = 110)
    dev <- grDevices::dev.cur()
    dessin <- FALSE
    setHook("plot.new", function() dessin <<- TRUE, "replace")
    setHook("grid.newpage", function() dessin <<- TRUE, "replace")
    ancien <- options(width = 110)
    for (e in exprs) {
      sortie <- character(0)
      res <- withCallingHandlers(
        tryCatch({
          sortie <- utils::capture.output(v <- withVisible(eval(e, console)))
          if (v$visible) sortie <- c(sortie, utils::capture.output(print(v$value)))
          NULL
        }, error = function(err) err),
        warning = function(w) { noter("avertissement", conditionMessage(w)); invokeRestart("muffleWarning") },
        message = function(msg) { noter("sortie", sub("\n$", "", conditionMessage(msg))); invokeRestart("muffleMessage") })
      if (length(sortie) > 3000) sortie <- c(utils::head(sortie, 3000), sprintf("... (%d lignes de plus)", length(sortie) - 3000))
      if (length(sortie)) noter("sortie", sortie)
      if (inherits(res, "error")) {
        noter("erreur", conditionMessage(res))
        break
      }
    }
    options(ancien)
    setHook("plot.new", NULL, "replace")
    setHook("grid.newpage", NULL, "replace")
    if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)
    images <- list.files(dirname(motif), sprintf("^console-%d-\\d+\\.png$", depart + 1), full.names = TRUE)
    if (dessin) for (f in images) noter("image", paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.size(f)))))
    unlink(images)
  }
  modifies <- etat$console_modifies
  etat$console_modifies <- character(0)
  list(journal = journal_depuis(depart), modifies = I(modifies))
}
