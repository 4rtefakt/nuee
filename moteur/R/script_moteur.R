# L'export vers R d'un calcul fait par des fonctions du moteur (TRIM, l'échantillonnage par distances,
# les arbres boostés, les forêts aléatoires : trim_script.R, distance_script.R, boosting_script.R,
# forets_script.R ; analyses.R : ecrire_script_<type>).
# Ces fonctions sont écrites dans le script telles que le moteur les a, relues dans leur fichier
# (moteur/R, le dossier que charger.R note), pour que le script refasse le calcul de Nuée lui-même
# et non une seconde écriture. Pour R seul, chacune est réécrite ainsi :
#   - un message traduire(fr, en) devient la chaîne de la langue du script (les fonctions qu'il
#     reprend écrivent leurs messages ainsi, sans assistant qui recevrait les deux langues) ;
#   - en français, elle garde ses commentaires (celui qui la précède dans son fichier et ceux de son
#     corps) ; en anglais, celui qui la précède est remplacé par sa traduction (`entetes`), ceux de son
#     corps aussi (`commentaires`, ligne par ligne), ou retirés s'ils n'en ont pas ;
#   - les fonctions et les constantes du moteur qu'elle utilise viennent avec elle
#     (dependances_moteur), dans l'ordre de leurs fichiers.
# Puis les noms du code passent en anglais pour un script en anglais (traduire_script, d'après la
# table du calcul), et le script s'écrit comme ceux des autres analyses (ecrire_script_moteur) : ses
# quatre parties, les fichiers d'à côté, ou un rapport R Markdown ou Quarto. Avant d'être écrit, son
# code est exécuté ici, sans le moteur, et ses résultats comparés à ceux du modèle de l'application.

SOURCES_MOTEUR <- new.env(parent = emptyenv())  # (les fichiers du moteur déjà lus)

# Le dossier des fichiers du moteur (moteur/R).
dossier_sources_moteur <- function() {
  d <- getOption("nuee.moteur")
  r <- if (length(d)) file.path(d[1], "R")
  if (is.null(r) || !dir.exists(r)) {
    stop(traduire("L'export vers R relit les fonctions du moteur dans ses fichiers (moteur/R), introuvables ici.",
                  "Export to R reads the engine's functions in its files (moteur/R), which can't be found here."), call. = FALSE)
  }
  r
}

# Chaque nom écrit en tête de ligne d'un fichier du moteur (« nom <- … »), et les fichiers où il
# l'est (une ligne de code dans une chaîne aussi, que definition_moteur écarte ensuite).
index_sources_moteur <- function() {
  dossier <- dossier_sources_moteur()
  if (identical(SOURCES_MOTEUR$dossier, dossier) && !is.null(SOURCES_MOTEUR$index)) return(SOURCES_MOTEUR$index)
  motif <- "^(`[^`]+`|[A-Za-z.][A-Za-z0-9._]*)[ ]*<-"
  index <- list()
  for (f in sort(list.files(dossier, "\\.R$", full.names = TRUE))) {
    l <- grep(motif, readLines(f, encoding = "UTF-8", warn = FALSE), value = TRUE)
    for (n in unique(gsub("`", "", sub(paste0(motif, ".*$"), "\\1", l)))) index[[n]] <- c(index[[n]], f)
  }
  SOURCES_MOTEUR$dossier <- dossier
  SOURCES_MOTEUR$index <- index
  SOURCES_MOTEUR$fichiers <- list()
  index
}

# Les définitions du haut d'un fichier du moteur (`nom <- valeur`) : leurs lignes, le commentaire qui
# les précède (les lignes de commentaire juste au-dessus) et leur ligne dans le fichier.
definitions_fichier_moteur <- function(f) {
  if (!is.null(SOURCES_MOTEUR$fichiers[[f]])) return(SOURCES_MOTEUR$fichiers[[f]])
  lignes <- readLines(f, encoding = "UTF-8", warn = FALSE)
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"))
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  enfants <- split(seq_len(nrow(pd)), pd$parent)
  de <- function(id) { k <- enfants[[as.character(id)]]; if (is.null(k)) pd[0, ] else pd[k, , drop = FALSE] }
  defs <- list()
  for (i in which(pd$parent == 0 & pd$token == "expr")) {
    e <- de(pd$id[i])
    if (nrow(e) < 3 || !e$token[2] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) next
    cible <- de(e$id[1])
    if (nrow(cible) != 1 || cible$token != "SYMBOL") next
    nom <- gsub("`", "", cible$text)
    if (!is.null(defs[[nom]])) next
    debut <- pd$line1[i]
    k <- debut
    while (k > 1 && grepl("^\\s*#", lignes[k - 1])) k <- k - 1
    defs[[nom]] <- list(nom = nom, fichier = f, ligne = debut, lignes = lignes[debut:pd$line2[i]],
                        commentaire = if (k < debut) lignes[k:(debut - 1)] else character(0))
  }
  SOURCES_MOTEUR$fichiers[[f]] <- defs
  defs
}

# La définition d'un nom en tête d'un fichier du moteur (la première, dans l'ordre des fichiers) ;
# NULL quand il n'en a pas (`exiger` : une erreur).
definition_moteur <- function(nom, exiger = TRUE) {
  for (f in index_sources_moteur()[[nom]]) {
    def <- definitions_fichier_moteur(f)[[nom]]
    if (!is.null(def)) return(def)
  }
  if (exiger) stop(sprintf(traduire("Export vers R : la définition de %s est introuvable dans le moteur.", "Export to R: the definition of %s can't be found in the engine."), nom), call. = FALSE)
  NULL
}

# Les noms qu'utilise une définition (ses lignes) : les fonctions qu'elle appelle et les symboles
# qu'elle lit, sans ceux qui suivent $, @ ou :: (un élément, un paquet), ni ses arguments, ni ce
# qu'elle définit elle-même (ses affectations, ses variables de boucle).
noms_utilises <- function(lignes) {
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"))
  pd <- pd[pd$terminal, , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  n <- nrow(pd)
  avant <- c("", pd$token[-n])
  apres <- c(pd$token[-1], "")
  utilises <- pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SPECIAL") & !avant %in% c("'$'", "'@'", "NS_GET", "NS_GET_INT")]
  boucles <- which(pd$token == "FOR") + 2
  locaux <- c(pd$text[pd$token == "SYMBOL_FORMALS"], pd$text[pd$token == "SYMBOL" & apres %in% c("LEFT_ASSIGN", "EQ_ASSIGN")],
              pd$text[boucles[boucles <= n]])
  setdiff(unique(gsub("`", "", utilises)), gsub("`", "", locaux))
}

# Les définitions du moteur qu'il faut pour `racines` : elles, et ce qu'elles utilisent du moteur,
# de proche en proche (sans `exclus` : traduire, dont les appels sont remplacés ; ni un nom qui
# n'a de définition que dans une chaîne).
dependances_moteur <- function(racines, exclus = character(0)) {
  index <- index_sources_moteur()
  exclus <- c("traduire", exclus)
  vus <- character(0)
  a_voir <- racines
  while (length(a_voir)) {
    n <- a_voir[1]
    a_voir <- a_voir[-1]
    if (n %in% vus) next
    vus <- c(vus, n)
    utilises <- setdiff(intersect(noms_utilises(definition_moteur(n)$lignes), names(index)), c(vus, exclus))
    a_voir <- c(a_voir, Filter(function(u) !is.null(definition_moteur(u, exiger = FALSE)), utilises))
  }
  vus
}

# Les appels traduire(fr, en) des `lignes` remplacés par le texte de leur argument dans la langue
# du script (une chaîne, le plus souvent).
remplacer_traductions <- function(lignes, langue) {
  texte <- paste(lignes, collapse = "\n")
  pd <- utils::getParseData(parse(text = texte, keep.source = TRUE, encoding = "UTF-8"))
  noms <- pd$parent[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "traduire"]
  if (!length(noms)) return(lignes)
  appels <- pd$parent[match(noms, pd$id)]
  debuts <- c(0, cumsum(nchar(strsplit(texte, "\n", fixed = TRUE)[[1]]) + 1))
  position <- function(ligne, colonne) debuts[ligne] + colonne
  morceaux <- lapply(appels, function(a) {
    k <- pd[pd$parent == a & pd$token == "expr", , drop = FALSE]
    k <- k[order(k$line1, k$col1), , drop = FALSE]
    arg <- k[if (identical(langue, "en")) 3 else 2, ]
    appel <- pd[pd$id == a, ]
    list(de = position(appel$line1, appel$col1), a = position(appel$line2, appel$col2),
         texte = substr(texte, position(arg$line1, arg$col1), position(arg$line2, arg$col2)))
  })
  for (x in morceaux[order(-vapply(morceaux, `[[`, 0, "de"))]) {
    texte <- paste0(substr(texte, 1, x$de - 1), x$texte, substr(texte, x$a + 1, nchar(texte)))
  }
  strsplit(texte, "\n", fixed = TRUE)[[1]]
}

# Les commentaires des `lignes` en anglais : chacun remplacé par sa traduction (`traductions` : le
# commentaire tel qu'il est écrit, « # … », et son texte anglais), sinon retiré, sa ligne avec lui
# s'il était seul dessus.
commentaires_anglais <- function(lignes, traductions = character(0)) {
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"))
  com <- pd[pd$token == "COMMENT", , drop = FALSE]
  garder <- rep(TRUE, length(lignes))
  for (i in order(-com$line1)) {
    l <- lignes[com$line1[i]]
    avant <- substr(l, 1, com$col1[i] - 1)
    if (com$text[i] %in% names(traductions)) lignes[com$line1[i]] <- paste0(avant, traductions[[com$text[i]]])
    else if (!nzchar(trimws(avant))) garder[com$line1[i]] <- FALSE
    else lignes[com$line1[i]] <- sub("\\s+$", "", avant)
  }
  lignes[garder]
}

# Le texte d'une définition du moteur pour le script (definition_moteur), dans sa langue : son
# commentaire et ses lignes (voir en tête) ; `entetes` : le commentaire anglais de chaque définition.
texte_moteur <- function(def, langue, entetes = character(0), commentaires = character(0)) {
  lignes <- remplacer_traductions(def$lignes, langue)
  if (!identical(langue, "en")) return(c(def$commentaire, lignes))
  c(if (def$nom %in% names(entetes)) commentaire(entetes[[def$nom]]), commentaires_anglais(lignes, commentaires))
}

# Le code des définitions du moteur qu'il faut pour `racines` (dependances_moteur), dans l'ordre de
# leurs fichiers, chacune suivie d'une ligne vide.
code_moteur <- function(racines, langue, entetes = character(0), commentaires = character(0), exclus = character(0)) {
  defs <- lapply(dependances_moteur(racines, exclus), definition_moteur)
  ordre <- order(vapply(defs, function(x) basename(x$fichier), ""), vapply(defs, `[[`, 0, "ligne"))
  unlist(lapply(defs[ordre], function(x) c(texte_moteur(x, langue, entetes, commentaires), "")))
}

# Le code d'un script dans sa langue : les lignes de commentaire « #fr » ou « #en » de l'autre
# langue retirées (celles de la sienne gardées, « # »), et pour un script en anglais, ses noms
# (fonctions, variables, arguments, éléments de listes) remplacés d'après `noms`. Les chaînes ne
# changent pas (les noms des colonnes des données, les valeurs que le code compare sont les mêmes
# dans les deux langues), sauf avec `elements` celles qui nomment un élément, x[["nom"]] : dans les
# fonctions du moteur, où elles ne sont jamais une colonne des données.
traduire_script <- function(lignes, langue, noms = character(0), elements = FALSE) {
  lignes <- unlist(lapply(lignes, function(l) if (grepl("\n", l, fixed = TRUE)) strsplit(l, "\n", fixed = TRUE)[[1]] else l))
  lignes <- gsub("\r", " ", lignes)  # (par sûreté : un « \r » isolé finirait la ligne quand on source le script)
  if (!length(lignes)) return(lignes)
  en <- identical(langue, "en")
  lignes <- lignes[!grepl(sprintf("^\\s*#%s( |$)", if (en) "fr" else "en"), lignes)]
  lignes <- sub(sprintf("^(\\s*)#%s( |$)", if (en) "en" else "fr"), "\\1#\\2", lignes)
  if (!en || !length(noms)) return(lignes)
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"), includeText = TRUE)
  pd <- pd[pd$terminal, , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  pd$nouveau <- NA_character_
  k <- pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB", "SYMBOL_FORMALS") & pd$text %in% names(noms)
  pd$nouveau[k] <- unname(noms[pd$text[k]])
  if (elements) {
    n <- nrow(pd)
    nom <- ifelse(pd$token == "STR_CONST", substr(pd$text, 2, nchar(pd$text) - 1), "")
    k <- pd$token == "STR_CONST" & c("", pd$token[-n]) == "LBB" & c(pd$token[-1], "") == "']'" & nom %in% names(noms)
    pd$nouveau[k] <- sprintf("\"%s\"", unname(noms[nom[k]]))
  }
  pd <- pd[!is.na(pd$nouveau), , drop = FALSE]
  pd <- pd[order(pd$line1, -pd$col1), , drop = FALSE]
  for (i in seq_len(nrow(pd))) {
    l <- lignes[pd$line1[i]]
    lignes[pd$line1[i]] <- paste0(substr(l, 1, pd$col1[i] - 1), pd$nouveau[i], substr(l, pd$col2[i] + 1, nchar(l)))
  }
  lignes
}

# Les noms du code `lignes` que la table `noms` ferait se confondre en anglais : chaque nom anglais
# qu'auraient deux variables ou fonctions différentes (deux noms français, ou un nom français et un
# nom qui reste tel quel, une fonction de R par exemple), avec ces noms ; une liste vide si aucun
# (les vérifications s'en servent, pour qu'un script en anglais calcule ce que calcule celui en
# français). Les noms des éléments d'une liste (après $, ou nommant un argument) n'y comptent pas :
# deux listes peuvent avoir chacune un élément du même nom.
collisions_traduction <- function(lignes, noms) {
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"))
  pd <- pd[pd$terminal, , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  avant <- c("", pd$token[-nrow(pd)])
  ids <- unique(pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS") & !avant %in% c("'$'", "'@'", "NS_GET", "NS_GET_INT")])
  anglais <- ifelse(ids %in% names(noms), unname(noms[ids]), ids)
  doubles <- unique(anglais[duplicated(anglais)])
  stats::setNames(lapply(doubles, function(x) ids[anglais == x]), doubles)
}

# Le nom d'un objet du script dans sa langue (d'après `noms`, puis ceux de script.R).
nom_traduit <- function(x, langue, noms) {
  if (!identical(langue, "en")) return(x)
  vapply(x, function(n) if (n %in% names(noms)) noms[[n]] else nom_script(n, langue), "", USE.NAMES = FALSE)
}

# Les lignes de la définition `d` dans le tableau du script `tab` (ses observations cochées, ou
# celles qu'en choisit sa condition : condition.R), puis `d`, le tableau de ces lignes (le calcul
# laisse ensuite de côté celles qu'il ne peut pas prendre, comme le moteur).
code_lignes_definition <- function(projet, d, tab, tx) {
  obs <- as.numeric(d$observations)
  rangs <- unname(tab$rangs[nom_numero(obs)])
  pc <- plan_condition(projet, d, tab)
  if (!is.null(pc)) {
    return(c(code_condition(pc, tx, tx(sprintf("Les %d observations du modèle.", length(obs)), sprintf("The %d observations of the model.", length(obs)))),
             "d <- donnees[lignes, ]"))
  }
  if (identical(as.integer(rangs), seq_len(nrow(tab$donnees)))) {
    return(c(commentaire(tx(sprintf("Les %d observations du tableau.", length(obs)), sprintf("The %d observations of the table.", length(obs)))), "d <- donnees"))
  }
  c(commentaire(tx(sprintf("Les %d observations du modèle (leurs rangs dans le tableau).", length(obs)), sprintf("The %d observations of the model (their rows in the table).", length(obs)))),
    code_valeurs(morceaux_rangs(rangs), "lignes <- "), "d <- donnees[lignes, ]")
}

# Une ligne de la partie « Résultats » : le code, puis son commentaire aligné.
ligne_sortie <- function(code, commentaire_ligne) paste0(formatC(code, width = -max(34, nchar(code) + 2)), "# ", commentaire_ligne)

# ---------------------------------------------------------------- écrire le script

# Le script (ou le rapport) d'une analyse faite par des fonctions du moteur. `piece` :
#   quoi         l'analyse, en mots (en-tête) ;
#   tab          le tableau du script (script.R : tableau_script) ;
#   fonctions, calcul, sortie : les lignes des parties 1, 3 et 4 (déjà dans la langue du script) ;
#   graphiques   une liste de list(nom, largeur, hauteur, code) ;
#   ecart        function(m, e) : NULL, ou ce qui diffère entre les résultats du script (dans `e`) et `m` ;
#   paquets      les paquets que le script appelle (renv.lock) ; `texte_paquets` : la phrase de
#                l'en-tête qui dit avec quoi il refait l'analyse ;
#   parties      les deux lignes de l'en-tête qui disent ce que font ses parties 1 et 3 ;
#   resultats    les blocs de tableaux d'un rapport (des list(nom, objet, chiffres)) ;
#   objets       ce que le .RData garde.
ecrire_script_moteur <- function(projet, d, chemin_script, m, piece, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  format <- c(r = "R", rmd = "Rmd", qmd = "qmd")[[tolower(tools::file_ext(chemin_script))]]
  fichiers <- fichiers_script(chemin_script, donnees, rdata, renv, rproj, langue)
  code_donnees <- donnees_script(piece$tab, fichiers, langue)
  e <- environnement_script(piece$tab, piece$fonctions, langue)
  probleme <- evaluer_script(piece$calcul, e, avertissements = FALSE)
  ecart <- if (!is.null(probleme)) probleme else piece$ecart(m, e)
  nom <- texte_sur(as.character(d$name %||% ""))
  titre <- texte_sur(as.character(d$title %||% ""))
  entete <- c(sprintf(tx("# Modèle %s%s : %s", "# Model %s%s: %s"), nom, if (nzchar(titre)) sprintf(" (%s)", titre) else "", piece$quoi),
              sprintf(tx("# Script R écrit par Nuée %s le %s, à partir du projet « %s ».", "# R script written by Nuée %s on %s, from the project “%s”."), version, format(Sys.Date()),
                      texte_sur(as.character(projet$source %||% "projet"))),
              commentaire(piece$texte_paquets),
              piece$parties[1],
              tx("#   2. Données", "#   2. Data"),
              piece$parties[2],
              tx("#   4. Résultats : tableaux et graphiques", "#   4. Results: tables and plots"),
              tx("# Dans RStudio, Ctrl+Maj+O affiche ces parties ; Ctrl+Maj+S lance tout le script.", "# In RStudio, Ctrl+Shift+O shows these parts; Ctrl+Shift+S runs the whole script."),
              sprintf(tx("# Écrit avec %s.", "# Written with %s."), texte_paquets(piece$paquets, langue, versions = TRUE)),
              fichiers_a_cote(fichiers, langue, FALSE))
  lignes <- if (format == "R") {
    c(entete, "", titre_partie(1, langue), piece$fonctions, titre_partie(2, langue), code_donnees, "", titre_partie(3, langue), piece$calcul, "",
      titre_partie(4, langue), piece$sortie, "", texte_graphiques(langue), unlist(lapply(piece$graphiques, function(g) c(g$code, ""))))
  } else {
    lignes_rapport_moteur(format, d, langue, texte_methodes(projet, d, m, langue, version), piece, code_donnees)
  }
  ecrire_fichiers_script(lignes, fichiers, e, c(nom_script("donnees", langue), piece$objets), c(piece$paquets, if (format != "R") c("knitr", "rmarkdown")))
  # (ses graphiques, pour un document reproductible : document_reproductible.R)
  structure(list(script = chemin_script, chemins = unname(fichiers), ecart = ecart), graphiques = piece$graphiques)
}

# Le rapport R Markdown ou Quarto : les méthodes, les données, le code (replié dans le rapport
# compilé), les résultats (tableaux, graphiques en R de base) et les références.
lignes_rapport_moteur <- function(format, d, langue, methodes, piece, code_donnees) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  bloc <- function(options, code) c(sprintf("```{r%s}", if (nzchar(options)) paste0(" ", options) else ""), code, "```", "")
  nom <- texte_sur(as.character(d$name %||% ""))
  titre <- sprintf("%s %s%s", tx("Modèle", "Model"), nom, if (nzchar(as.character(d$title %||% ""))) paste(" —", texte_sur(d$title)) else "")
  yaml <- c("---", sprintf("title: %s", code_texte(titre)), sprintf("date: %s", code_texte(format(Sys.Date()))), sprintf("lang: %s", langue),
            if (format == "Rmd") c("output:", "  html_document:", "    code_folding: hide") else c("format:", "  html:", "    embed-resources: true", "    code-fold: true"),
            "---", "")
  resultats <- unlist(lapply(piece$resultats, function(r) bloc(r$nom, sprintf("knitr::kable(%s, digits = %d)", r$objet, r$chiffres))))
  c(yaml,
    sprintf("## %s", tx("Méthodes", "Methods")), "", rbind(texte_sur(methodes$paragraphes), ""),
    sprintf("## %s", tx("Données", "Data")), "",
    bloc("donnees, echo = FALSE", code_donnees),
    bloc("", traduire_code("str(donnees, list.len = 12)", langue)),
    sprintf("## %s", tx("Modèle", "Model")), "",
    tx("Le calcul, écrit en R pour ce modèle (bouton « Code » pour le voir).", "The computation, written in R for this model (“Code” button to see it)."), "",
    bloc("fonctions", piece$fonctions[seq_len(max(0, length(piece$fonctions) - 1))]),
    bloc("modele", piece$calcul),
    sprintf("## %s", tx("Résultats", "Results")), "",
    resultats,
    unlist(lapply(piece$graphiques, function(g) bloc(sprintf("%s, fig.width = %s, fig.height = %s", g$nom, g$largeur, g$hauteur), g$code))),
    sprintf("## %s", tx("Références", "References")), "", paste("-", methodes$references))
}
