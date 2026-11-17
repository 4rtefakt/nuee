# Les modèles qui ne sont pas faits de composantes NIPALS (ACP, PLS, OPLS) : les régressions
# (regression.R) et les autres analyses, chacune dans son fichier : analyse de redondance (RDA)
# et analyse des correspondances (CA), analyse discriminante linéaire (LDA), arbre de décision
# (TREE), k-moyennes (KMEANS), indices et tendances de comptages (TRIM, trim.R), forêt aléatoire
# (FOREST, forets.R), arbres boostés (BOOST, boosting.R), échantillonnage par distances (DISTANCE,
# distance.R), un modèle écrit en code R par l'utilisateur (CODE, code.R et code_script.R), un modèle
# fait d'un greffon (PLUGIN, greffons.R). Leur type est celui de definir_modele() ; leurs réglages
# propres sont dans d$options.
#
# Chaque analyse suit la même convention de noms, pour que le moteur et le serveur la trouvent
# sans liste à tenir, <type> en minuscules (ajuster_rda, resultat_kmeans...) :
#   definir_<type>(projet, d)        sa définition complétée ou refusée par definir_modele (nouveau.R),
#                                    si elle en a besoin (CODE : son code et les colonnes qu'il nomme) ;
#   ajuster_<type>(projet, d)        l'ajuste (modele.R : ajuster) ; l'objet rendu a son $type ;
#   resultat_<type>(id)              ce que l'interface en reçoit (serveur.R : resultat_modele),
#                                    avec analyse = TRUE, prediction = TRUE ou FALSE (prédit-elle de
#                                    nouvelles observations ?) et chiffres (sa ligne dans l'explorateur) ;
#   texte_methodes_<type>(projet, d, m, langue, version)   son texte des méthodes (methodes.R) ;
#   tables_<type>(projet, m)         ses tableaux pour Exporter (formats.R : tables_resultats) ;
#   predire_<type>(projet, m, d, observations)             ses prédictions (prediction.R), si elle prédit ;
#   tester_<type>(projet, m, d)      son jeu de test (d$test, jeu_test.R), si elle en prend un : ses
#                                    observations prédites, rangées dans m$test ; sans cette
#                                    fonction, definir_modele() refuse un jeu de test ;
#   ecrire_script_<type>(projet, d, chemin_script, m, donnees, renv, rdata, rproj, version, langue)
#                                    son export vers R (script.R : ecrire_script), rendu comme
#                                    ecrire_script : list(script, chemins, ecart) ;
#   demande_analyse_<nom>(q)         une demande de l'interface qui lui est propre, <nom> (des
#                                    tests par permutation calculés à la demande...) : serveur.R
#                                    l'ajoute à ses DEMANDES ;
#   verifier_<type>(projet, d)       à la place de la vérification des données avant l'ajustement
#                                    (diagnostic.R : verifier_donnees), si elle n'a pas de sens pour
#                                    elle (des comptages manquants que TRIM impute) ;
#   entrees_<type>(projet, d, m)     ses résultats clés, pour la vue « Ce qui a changé » (écrite dans
#                                    resultats_cles.R, avec celles des autres types ; sans elle, le
#                                    nombre de ses observations seulement).
# Leurs fichiers ne commencent pas par « a » : chargés après celui-ci (moteur/charger.R).
# Ses variables Z (covariables d'une analyse partielle), s'il en a, sont dans d$options$z. Les
# demandes qui supposent des composantes et leurs scores (serveur.R : SANS_REGRESSION) lui sont
# refusées, comme aux régressions ; le script de toute une séance la laisse de côté (elle
# s'exporte seule).
AUTRES_ANALYSES <- c("RDA", "CA", "LDA", "TREE", "KMEANS", "CCA", "INDVAL", "TRIM", "PCOA", "NMDS", "PERMANOVA", "MANTEL", "FOREST", "BOOST", "DISTANCE", "CODE", "PLUGIN")  # (CCA : ordination.R ; espèces indicatrices : indicatrices.R ; TRIM : trim.R ; PCoA, NMDS, PERMANOVA, Mantel : communautes.R ; forêts : forets.R ; arbres boostés : boosting.R ; échantillonnage par distances : distance.R ; code R : code.R ; greffons : greffons.R)

est_autre_analyse <- function(d) isTRUE(as.character(d$type %||% "") %in% AUTRES_ANALYSES)

# Un modèle sans composantes NIPALS : une régression ou une autre analyse.
sans_composantes <- function(d) est_regression(d) || est_autre_analyse(d)

# La fonction <prefixe><type> d'une analyse, ou NULL : une du moteur, que ses fichiers définissent
# (charger.R : nuee.noms_moteur), pas une fonction du même nom que l'utilisateur a écrite dans
# l'environnement global (un definir_trim de son script n'est pas la définition de TRIM).
fonction_analyse <- function(prefixe, type) {
  nom <- paste0(prefixe, tolower(as.character(type)[1]))
  noms <- getOption("nuee.noms_moteur")
  if (is.environment(noms) && !exists(nom, envir = noms, inherits = FALSE)) return(NULL)
  get0(nom, mode = "function")
}

ajuster_analyse <- function(projet, d) {
  f <- fonction_analyse("ajuster_", d$type)
  if (is.null(f)) stop(sprintf(traduire("Analyse inconnue : %s", "Unknown analysis: %s"), d$type), call. = FALSE)
  m <- f(projet, d)
  if (length(d$test)) m$test <- fonction_analyse("tester_", d$type)(projet, m, d)
  m
}

predire_analyse <- function(projet, m, d, observations) {
  f <- fonction_analyse("predire_", d$type)
  if (is.null(f)) stop(traduire("Cette analyse ne prédit pas de nouvelles observations.", "This analysis doesn't predict new observations."), call. = FALSE)
  f(projet, m, d, observations)
}

ecrire_script_analyse <- function(projet, d, chemin_script, m, donnees, renv, rdata, rproj, version, langue) {
  f <- fonction_analyse("ecrire_script_", d$type)
  if (is.null(f)) stop(traduire("Pas encore d'export vers R pour cette analyse.", "No export to R for this analysis yet."), call. = FALSE)
  f(projet, d, chemin_script, m %||% ajuster(projet, d), donnees, renv, rdata, rproj, version, langue)
}
