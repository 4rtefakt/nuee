# Écrit un script R qui refait un modèle avec les fonctions de ce moteur, pour le relire, le
# modifier et le relancer dans RStudio. Les données du projet sont écrites à côté du script.

ecrire_script <- function(projet, d, chemin_script, dossier_moteur) {
  chemin_donnees <- sub("\\.[Rr]$", "", chemin_script)
  chemin_donnees <- paste0(chemin_donnees, " - donnees.json")
  enregistrer_projet(projet, chemin_donnees)
  q <- function(x) deparse(x, width.cutoff = 500L)
  lignes <- c(
    sprintf("# Modèle %s%s", d$name %||% "", if (nzchar(d$title %||% "")) sprintf(" : %s", d$title) else ""),
    sprintf("# Script écrit par Nuée le %s. Il refait le modèle avec les fonctions", format(Sys.Date())),
    "# du moteur (en R, commentées en français, dans le dossier moteur/R) ; on peut le modifier",
    "# et le relancer, par exemple dans RStudio. Il demande les paquets jsonlite et openxlsx :",
    "# install.packages(c(\"jsonlite\", \"openxlsx\"))",
    "",
    sprintf("source(%s)", q(file.path(dossier_moteur, "charger.R"))),
    sprintf("p <- lire_projet(%s)  # les données du projet, écrites à côté de ce script", q(chemin_donnees)),
    "")
  lignes <- c(lignes, code_definition(projet, d, "p"))
  pls <- identical(d$type, "PLS")
  lignes <- c(lignes, "",
    "m <- ajuster(p, d)",
    "",
    "# Résultats (les mêmes que dans l'application)",
    "m$statistiques$composantes         # R2X, R2Y, Q2 et valeur propre par composante",
    "m$statistiques$variables           # loadings, VIP, intervalles de confiance",
    if (pls) "m$statistiques$coefficients        # coefficients (centrés-réduits et dans les unités des données)",
    "head(m$statistiques$observations)  # scores, T2, DModX, valeurs ajustées",
    if (pls) "m$statistiques$cv_anova           # CV-ANOVA",
    if (pls) "# permutations(m, n = 20)          # test de permutation (quelques secondes)",
    if (isTRUE(d$plsda)) "m$statistiques$classification$vc$table  # table de classification (validation croisée)")
  writeLines(enc2utf8(lignes), chemin_script, useBytes = TRUE)
  list(script = chemin_script, donnees = chemin_donnees)
}

# Le code R qui crée la définition `d` d'un modèle, le projet s'appelant `p` dans ce code :
# modele_du_projet() pour un modèle importé d'un fichier .usp, definir_modele() sinon, avec les variables
# désignées par leur nom (pour le script R et pour le journal de la console).
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
      "",
      sprintf("d <- definir_modele(%s, %s, x = x, y = y, classe = classe, observations = observations,", p, q(r$type)),
      "                    echelle = echelles, transformations = transformations, croises = croises,",
      sprintf("                    A = %s, G = %s, nom = %s, titre = %s)", q(r$A), q(r$G), q(r$nom), q(r$titre)))
  }
}
