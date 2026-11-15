# L'export vers R des analyses de communautés fondées sur une distance (communautes.R : PCoA, NMDS,
# PERMANOVA et dispersion multivariée, test de Mantel et corrélogramme) et des espèces indicatrices
# (indicatrices.R) : analyses.R, ecrire_script_<type> ; script.R pour les autres modèles. Un script
# qui refait l'analyse avec R seul (et le paquet MASS, livré avec R, pour un NMDS), sans Nuée : ses
# données, ses variables comme le moteur les lit (onglet Variables : transformations, catégories ;
# la mise à l'échelle des X d'un test de Mantel), le calcul, ses résultats et des graphiques en R de
# base ; ou un rapport R Markdown ou Quarto qui y ajoute le texte des méthodes et les références.
# Ses tests par permutation ont les permutations de Nuée (la même graine) : les mêmes valeurs p.
#
# Les fonctions du script sont celles du moteur, telles qu'elles sont écrites dans ses fichiers
# (code_moteur : sans leurs commentaires, chaque message de traduire() dans la langue du script),
# chacune précédée d'un commentaire dans la langue du script ; le modèle, lui, est écrit pour ce
# modèle. Avant d'être écrit, le code est exécuté ici, sans le moteur, et ses résultats comparés à
# ceux du modèle de l'application : un écart est renvoyé dans `ecart`. Les noms du code sont en
# français, traduits pour un script en anglais (NOMS_COMMUNAUTES_EN, puis ceux de
# ordination_script.R et de script.R).

# (ni le nom d'un argument d'une fonction de R que le script appelle, comme `base` de log(x, base = 2),
# ni celui d'une fonction de R : une variable « par » traduite en « by » casserait par(), une
# variable « classe » traduite en « class » masquerait class())
NOMS_COMMUNAUTES_EN <- c(
  # les distances et les transformations
  transformer_communaute = "transform_table", relatives = "relative", methode = "method", distance_tableau = "table_distance",
  binaire = "binary", noms = "labels", etendue = "ranges", varie = "varies", somme = "sums", nz = "nonzero",
  centrage_double = "double_centring", corriger_distance = "correct_distance", hors = "off_diagonal", constante = "constant",
  corr = "corrected", nom = "name", niveau = "level",
  # la PCoA et le NMDS
  pcoa_noyau = "pcoa_core", garde = "kept", valeurs = "values", positifs = "positive", signes = "signs",
  table_valeurs_pcoa = "pcoa_eigenvalue_table", axe = "axis", valeur_propre = "eigenvalue", part = "share", cumul = "cumulative",
  scores_especes = "species_scores", mul = "expand", correlations_colonnes = "column_correlations",
  ajustement_environnement = "environmental_fit", vecteurs = "vectors", depasse = "exceeds", facteurs_fit = "factor_fit",
  intra = "within_ss", lignes = "rows", niveaux = "group_levels", centroides_facteurs = "factor_centroids",
  procrustes_symetrique = "symmetric_procrustes", residus = "resid", carres = "squares", avec_graine = "with_seed", graine = "seed",
  nmds_noyau = "nmds_core", dis = "dissimilarities", nuls = "zeros", depart = "start", departs = "starts", meilleur = "best",
  retrouvee = "found_again", ecarts = "deviations", tri = "sorted", ordre = "max_order", dissimilarite = "dissimilarity", ajustee = "fitted",
  stress_departs = "stress_starts", remplacement = "replacement", especes = "species", connues = "known", facteurs = "factors",
  facteurs_connus = "known_factors", ajustement = "fit", centroides = "centroids", variables_ajustees = "fitted_variables",
  facteurs_ajustes = "fitted_factors", complet = "complete_rows", etiquette_axe = "axis_label", fleches = "arrow_ends",
  multiplicateur = "multiplier", limites = "limits", valeurs_propres = "eigenvalues", couleurs = "colours", couleur = "colour",
  # la PERMANOVA et la dispersion
  permanova_noyau = "permanova_core", groupes = "groups", totale = "total", ddl_res = "df_residual", tk = "tukey_table",
  termes = "terms", gardes = "kept_terms", modele = "model", ajuste = "fitted_ss", Fp_modele = "Fp_model", testables = "testable",
  tete = "head_rows", terme = "term", ddl = "df", ddl_residus = "df_residuals", mediane_spatiale = "spatial_median",
  dispersion_noyau = "dispersion_core", groupe = "group", ecart = "deviation", ajust = "fit_lm", moyenne = "mean",
  mediane = "median", paire = "pair", bas = "lower", haut = "upper", termes_x = "x_terms", groupements = "groupings",
  groupement = "grouping", connus = "known", termes_table = "term_rows", contenu_dans = "contained_in",
  variables_groupe = "group_variables",
  # Mantel
  paires_triangle = "triangle_pairs", mantel_noyau = "mantel_core", partiel = "partial", perm_p = "perm_partial",
  r_partiel = "r_partial", p_partiel = "p_partial", correlogramme_noyau = "correlogram_core", debut = "first", fin = "last",
  nc = "n_classes", largeur = "width", bornes = "breaks", classe = "class_index", moitie = "half", paires = "n_pairs",
  complete = "complete_class", teste = "tested", rangs = "ranks", corrige = "corrected_p", p_corrige = "p_corrected",
  correlogramme = "correlogram", echelle_terme = "scale_term", et = "sds", poids = "weight", ponderation = "weighting",
  echelles_x = "scalings_x", ponderations_x = "weightings_x", echelles_z = "scalings_z", ponderations_z = "weightings_z",
  coordonnees = "coordinates", RAYON_TERRE = "EARTH_RADIUS", distances_entre_points = "point_distances", dp = "dphi", dl = "dlambda",
  # les espèces indicatrices
  combinaisons_groupes = "group_combinations", colonnes = "columns", statistiques_indval = "indval_statistics", sommes = "sums",
  parts = "shares", egalises = "equalized", meilleures_indval = "best_indval", valeur = "value", rang = "combination",
  test_indval = "indval_test", plus = "count_above", valeur_p_superieure = "upper_p_value", connu = "known", absentes = "absent",
  statistiques = "statistics", meilleures = "best", p_ajuste = "p_adjusted", espece = "species", ordre_especes = "species_order",
  vus = "shown")

# Les commentaires de la langue du script (« #fr » ou « #en » en tête de ligne), et les noms du
# code en anglais pour un script en anglais (comme traduire_ordination, avec ceux de ce fichier).
traduire_communautes <- function(lignes, langue) {
  lignes <- unlist(lapply(lignes, function(l) if (grepl("\n", l, fixed = TRUE)) strsplit(l, "\n", fixed = TRUE)[[1]] else l))
  lignes <- gsub("\r", " ", lignes)  # (par sûreté : un « \r » isolé finirait la ligne quand on source le script)
  if (!length(lignes)) return(lignes)
  autre <- if (identical(langue, "en")) "fr" else "en"
  lignes <- lignes[!grepl(sprintf("^\\s*#%s( |$)", autre), lignes)]
  lignes <- sub(sprintf("^(\\s*)#%s( |$)", if (identical(langue, "en")) "en" else "fr"), "\\1#\\2", lignes)
  if (!identical(langue, "en")) return(lignes)
  noms <- noms_communautes_en()
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"), includeText = TRUE)
  pd <- pd[pd$terminal & pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB", "SYMBOL_FORMALS") & pd$text %in% names(noms), ]
  pd <- pd[order(pd$line1, -pd$col1), ]
  for (i in seq_len(nrow(pd))) {
    l <- lignes[pd$line1[i]]
    lignes[pd$line1[i]] <- paste0(substr(l, 1, pd$col1[i] - 1), noms[[pd$text[i]]], substr(l, pd$col2[i] + 1, nchar(l)))
  }
  lignes
}
noms_communautes_en <- function() {
  noms <- c(NOMS_COMMUNAUTES_EN, NOMS_ORDINATION_EN, NOMS_SCRIPT_EN)
  noms[!duplicated(names(noms))]
}
nom_communautes <- function(x, langue) {
  if (!identical(langue, "en")) return(x)
  noms <- noms_communautes_en()
  vapply(x, function(n) if (n %in% names(noms)) noms[[n]] else n, "", USE.NAMES = FALSE)
}

# Un nom du script tiré des données (une variable : son nom, pour nommer un élément de liste) :
# tel quel si c'est un nom R que la traduction du script ne change pas, sinon entre accents graves.
nom_element <- function(nom) {
  if (identical(make.names(nom), nom) && !nom %in% names(noms_communautes_en())) return(nom)
  bt <- deparse(as.name(nom), backtick = TRUE)
  if (startsWith(bt, "`")) bt else paste0("`", nom, "`")
}

# ---------------------------------------------------------------- les fonctions du moteur, pour le script

# Le code d'une définition du moteur pour le script (telle qu'elle est écrite dans son fichier :
# definition_moteur, script_moteur.R) : sans ses commentaires (le script a les siens, dans sa
# langue), chaque traduire("…", "…") remplacé par le texte de la langue du script. (Les
# colonnes de getParseData y sont des caractères, comme pour substr : la séance est en UTF-8, comme
# le moteur le demande, serveur.R.)
code_definition_moteur <- function(nom, langue) {
  lignes <- definition_moteur(nom)$lignes
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"), includeText = TRUE)
  avant <- cumsum(c(0, nchar(lignes) + 1))  # (les caractères avant chaque ligne, les lignes jointes par "\n")
  debut <- function(i) avant[pd$line1[i]] + pd$col1[i]
  fin <- function(i) avant[pd$line2[i]] + pd$col2[i]
  de <- a <- integer(0)
  par <- character(0)
  for (i in which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "traduire")) {
    appel <- which(pd$id == pd$parent[pd$id == pd$parent[i]])
    chaines <- pd[pd$token == "STR_CONST" & pd$parent %in% pd$id[pd$parent == pd$id[appel]], , drop = FALSE]
    chaines <- chaines[order(chaines$line1, chaines$col1), , drop = FALSE]
    if (nrow(chaines) != 2) {
      stop(sprintf(traduire("Export vers R : %s appelle traduire() sans ses deux textes.", "Export to R: %s calls traduire() without its two texts."), nom), call. = FALSE)
    }
    de <- c(de, debut(appel))
    a <- c(a, fin(appel))
    par <- c(par, chaines$text[if (identical(langue, "en")) 2 else 1])
  }
  for (i in which(pd$token == "COMMENT")) {
    if (any(debut(i) >= de & fin(i) <= a)) next  # (un commentaire dans un appel déjà remplacé)
    de <- c(de, debut(i))
    a <- c(a, fin(i))
    par <- c(par, "")
  }
  texte <- paste(lignes, collapse = "\n")
  for (k in order(-de)) texte <- paste0(substr(texte, 1, de[k] - 1), par[k], substr(texte, a[k] + 1, nchar(texte)))
  sortie <- sub("[[:space:]]+$", "", strsplit(texte, "\n", fixed = TRUE)[[1]])
  sortie[nzchar(sortie)]
}

# Ce qu'en dit le script, avant chacune (en français, en anglais) ; une définition sans commentaire
# suit celle qui la présente.
DESCRIPTIONS_MOTEUR <- list(
  transformer_communaute = c(
    "Le tableau transformé avant la distance : racine carrée (racine2), racine quatrième (racine4), log(x + 1), présences-absences (1 pour une valeur positive), abondances relatives (chaque observation divisée par son total), double standardisation de Wisconsin (chaque variable divisée par son maximum, puis chaque observation par son total ; vegan::wisconsin), précédée ou non d'une racine carrée (racine_wisconsin). Une variable ou une observation sans aucune valeur reste nulle.",
    "The table transformed before the distance: square root (racine2), fourth root (racine4), log(x + 1), presence–absence (1 for a positive value), relative abundances (each observation divided by its total), Wisconsin double standardization (each variable divided by its maximum, then each observation by its total; vegan::wisconsin), preceded or not by a square root (racine_wisconsin). A variable or an observation without any value stays zero."),
  ZERO_DISTANCE = c(
    "La distance entre les lignes de Y, comme vegan::vegdist : bray, Σ|x − y| / Σ(x + y) (Bray et Curtis 1957) ; jaccard, 2B / (1 + B), B celle de Bray-Curtis ; kulczynski, 1 − ½ (Σ min(x, y) / Σx + Σ min(x, y) / Σy) ; gower, la moyenne des |x − y| / étendue de la variable, sur les variables qui varient (Gower 1971) ; euclidean, √Σ(x − y)² ; manhattan, Σ|x − y| ; canberra, la moyenne des |x − y| / (x + y) sur les variables non nulles dans l'une ou l'autre ; hellinger, euclidienne sur √(x / Σx) ; chord (corde), euclidienne sur x / √Σx² (Legendre et Gallagher 2001). `binaire` : sur les présences-absences (Bray-Curtis devient l'indice de Sørensen). Une distance sous ZERO_DISTANCE vaut 0, comme dans vegan.",
    "The distance between the rows of Y, as vegan::vegdist: bray, Σ|x − y| / Σ(x + y) (Bray and Curtis 1957); jaccard, 2B / (1 + B), B the Bray–Curtis one; kulczynski, 1 − ½ (Σ min(x, y) / Σx + Σ min(x, y) / Σy); gower, the mean of |x − y| / range of the variable, over the variables that vary (Gower 1971); euclidean, √Σ(x − y)²; manhattan, Σ|x − y|; canberra, the mean of |x − y| / (x + y) over the variables non-zero in either; hellinger, Euclidean on √(x / Σx); chord, Euclidean on x / √Σx² (Legendre and Gallagher 2001). `binary`: on presence–absence (Bray–Curtis becomes the Sørensen index). A distance below ZERO_DISTANCE is 0, as in vegan."),
  centrage_double = c("Le double centrage de Gower : les moyennes des colonnes, puis celles des lignes, retirées.",
                      "Gower's double centring: the column means, then the row means, removed."),
  corriger_distance = c(
    "La correction d'une distance dont la PCoA a des valeurs propres négatives (vegan::wcmdscale(add = ) ; Legendre et Legendre 2012, § 9.3.4) : Lingoes (1971), √(d² + 2c) hors de la diagonale, c la plus grande valeur absolue des valeurs propres négatives de −½ D² doublement centrée ; Cailliez (1983), d + c, c la plus grande valeur propre réelle de la matrice 2n × 2n [0, −Δ1 ; −I, Δ2] (Δ1 : D² doublement centrée, Δ2 : 2D doublement centrée). La distance corrigée et sa constante.",
    "The correction of a distance whose PCoA has negative eigenvalues (vegan::wcmdscale(add = ); Legendre and Legendre 2012, § 9.3.4): Lingoes (1971), √(d² + 2c) off the diagonal, c the largest absolute value of the negative eigenvalues of the doubly centred −½ D²; Cailliez (1983), d + c, c the largest real eigenvalue of the 2n × 2n matrix [0, −Δ1; −I, Δ2] (Δ1: D² doubly centred, Δ2: 2D doubly centred). The corrected distance and its constant."),
  pcoa_noyau = c(
    "L'analyse en coordonnées principales (vegan::wcmdscale ; Gower 1966) : les valeurs et vecteurs propres de G = −½ D² doublement centrée ; une valeur propre sous √ε fois la première (en valeur absolue) est nulle. Les coordonnées : les vecteurs × √|λ|, chaque axe orienté pour que sa plus grande coordonnée (en valeur absolue) soit positive ; `points`, celles des axes de valeur propre positive. L'inertie totale, la trace de G, est la somme de toutes les valeurs propres.",
    "The principal coordinates analysis (vegan::wcmdscale; Gower 1966): the eigenvalues and eigenvectors of G = −½ D² doubly centred; an eigenvalue below √ε times the first (in absolute value) is zero. The coordinates: the vectors × √|λ|, each axis turned so that its largest coordinate (in absolute value) is positive; `points`, those of the axes of positive eigenvalue. The total inertia, the trace of G, is the sum of all the eigenvalues."),
  table_valeurs_pcoa = c(
    "Les valeurs propres : leur part de l'inertie totale (la somme de toutes : avec des valeurs propres négatives, les axes positifs en portent plus de 100 %), leur cumul, négatives ou non.",
    "The eigenvalues: their share of the total inertia (the sum of all of them: with negative eigenvalues, the positive axes carry more than 100 %), their cumulative share, negative or not."),
  scores_especes = c(
    "Les scores des espèces (les colonnes de Y) : la moyenne des scores des observations pondérée par les valeurs (transformées) de chaque espèce, dilatée pour que leur variance pondérée soit celle des observations (vegan::wascores(expand = TRUE), comme metaMDS) ; seulement pour un tableau positif ou nul ; une espèce sans aucune valeur n'a pas de score.",
    "The scores of the species (the columns of Y): the mean of the observations' scores weighted by the (transformed) values of each species, expanded so that their weighted variance is that of the observations (vegan::wascores(expand = TRUE), as metaMDS); only for a positive or zero table; a species without any value has no score."),
  correlations_colonnes = c(
    "L'ajustement des variables X sur les axes S d'une ordination (vegan::envfit, vectorfit, factorfit) : pour une variable quantitative, sa régression sur les axes centrés, sa direction (les coefficients ramenés à une longueur 1) et son R² (le carré de la corrélation de la variable et de ses valeurs ajustées) ; pour une variable qualitative, R² = 1 − somme des carrés dans ses catégories / totale. Leur valeur p : la part des permutations P des valeurs de la variable qui font au moins aussi bien.",
    "The fit of the X variables onto the axes S of an ordination (vegan::envfit, vectorfit, factorfit): for a quantitative variable, its regression on the centred axes, its direction (the coefficients scaled to length 1) and its R² (the squared correlation of the variable and its fitted values); for a qualitative variable, R² = 1 − within-category sum of squares / total. Their p-value: the share of the permutations P of the variable's values that do at least as well."),
  centroides_facteurs = c(
    "Les centroïdes des catégories des variables qualitatives sur tous les axes (la moyenne des scores de leurs observations), une ligne par catégorie observée, « habitat(prairie) ».",
    "The centroids of the categories of the qualitative variables on all the axes (the mean of their observations' scores), one row per observed category, “habitat(meadow)”."),
  procrustes_symetrique = c(
    "La rotation symétrique de Procrustes de Y sur X (vegan::procrustes(symmetric = TRUE)) : les deux centrés, ramenés à une somme des carrés de 1 ; l'erreur quadratique moyenne et le résidu de chaque observation.",
    "The symmetric Procrustes rotation of Y onto X (vegan::procrustes(symmetric = TRUE)): both centred, scaled to a sum of squares of 1; the root mean square error and the residual of each observation."),
  nmds_noyau = c(
    "Le NMDS en k dimensions (MASS::isoMDS, la descente du stress de Kruskal 1964), comme vegan::metaMDS avec isoMDS : un premier ajustement depuis la configuration de la PCoA (cmdscale), puis `departs` configurations au hasard (uniformes sur [0, 1], graine fixe) ; un départ dont le stress ne dépasse pas celui du meilleur de 0,0005 en est comparé par Procrustes : plus bas, il devient le meilleur ; semblable (erreur quadratique moyenne < 0,005 et plus grand résidu < 0,01), la meilleure solution est « retrouvée ». Une dissimilarité nulle devient la moitié de la plus petite non nulle (metaMDS : zerodist = \"add\"). Puis la configuration est centrée, tournée sur ses axes principaux et mise à l'échelle de la plus grande dissimilarité (vegan::postMDS sans demi-changement), chaque axe orienté comme ceux de la PCoA ; le diagramme de Shepard et le stress de chaque observation (vegan::goodness).",
    "The NMDS in k dimensions (MASS::isoMDS, the descent of Kruskal's 1964 stress), as vegan::metaMDS with isoMDS: a first fit from the configuration of the PCoA (cmdscale), then `starts` random configurations (uniform on [0, 1], fixed seed); a start whose stress is within 0.0005 of the best one's is compared with it by Procrustes: lower, it becomes the best; similar (root mean square error < 0.005 and largest residual < 0.01), the best solution is “found again”. A zero dissimilarity becomes half the smallest non-zero one (metaMDS: zerodist = \"add\"). Then the configuration is centred, rotated to its principal axes and scaled to the largest dissimilarity (vegan::postMDS without half-change scaling), each axis turned as those of the PCoA; the Shepard diagram and the stress of each observation (vegan::goodness)."),
  variables_groupe = c(
    "Un terme qu'un terme croisé contient (A dans A*B ou A*A) n'est pas testé seul parmi les effets marginaux.",
    "A term contained in a cross term (A in A*B or A*A) isn't tested alone among the marginal effects."),
  permanova_noyau = c(
    "La PERMANOVA de G = −½ D² doublement centrée (vegan::adonis2 ; Anderson 2001, McArdle et Anderson 2001) sur les colonnes de X (centrées ; `groupes` : le terme de chaque colonne). La somme des carrés expliquée par des colonnes est tr(H G), H la projection sur elles ; celle des résidus, tr(G) − tr(H G) pour tous les termes. Les termes séquentiels (\"termes\", by = \"terms\") : chacun après ceux qui le précèdent ; marginaux (\"marges\", by = \"margin\") : chacun après tous les autres, sauf ceux qu'un terme croisé contient ; le modèle entier (\"modele\"). Le pseudo-F de chacun, (SC / ddl) / (SC résiduelle / ddl résiduels), est comparé à celui des permutations P des observations (les lignes et les colonnes de G permutées ensemble).",
    "The PERMANOVA of G = −½ D² doubly centred (vegan::adonis2; Anderson 2001, McArdle and Anderson 2001) on the columns of X (centred; `groups`: the term of each column). The sum of squares explained by columns is tr(H G), H the projection onto them; that of the residuals, tr(G) − tr(H G) for all the terms. Sequential terms (\"termes\", by = \"terms\"): each after those before it; marginal (\"marges\", by = \"margin\"): each after all the others, except those a cross term contains; the whole model (\"modele\"). The pseudo-F of each, (SS / df) / (residual SS / residual df), is compared with that of the permutations P of the observations (the rows and columns of G permuted together)."),
  mediane_spatiale = c(
    "La médiane spatiale des lignes de X (le point qui rend minimale la somme des distances euclidiennes), par BFGS depuis la médiane de chaque colonne (vegan::ordimedian).",
    "The spatial median of the rows of X (the point that minimizes the sum of Euclidean distances), by BFGS from the median of each column (vegan::ordimedian)."),
  dispersion_noyau = c(
    "La dispersion multivariée des groupes `groupe` d'une distance (vegan::betadisper ; Anderson 2006) : la PCoA de D (les axes de valeur propre négative compris), le centre de chaque groupe (son centroïde, ou sa médiane spatiale, calculée à part sur les axes positifs et sur les négatifs), la distance de chaque observation au centre de son groupe, √(d²+ − d²−) (d²− : sur les axes négatifs ; une différence négative devient 0, comptée) ; l'ANOVA de ces distances entre les groupes, son pseudo-F comparé à celui des permutations P de ses résidus (vegan::permutest.betadisper), et les paires de groupes de Tukey.",
    "The multivariate dispersion of the groups `group` of a distance (vegan::betadisper; Anderson 2006): the PCoA of D (the axes of negative eigenvalue included), the centre of each group (its centroid, or its spatial median, computed apart on the positive axes and on the negative ones), the distance of each observation to the centre of its group, √(d²+ − d²−) (d²−: on the negative axes; a negative difference becomes 0, counted); the ANOVA of these distances between the groups, its pseudo-F compared with that of the permutations P of its residuals (vegan::permutest.betadisper), and Tukey's pairs of groups."),
  paires_triangle = c("Les paires d'observations d'une matrice n × n (le triangle inférieur, colonne par colonne).",
                      "The pairs of observations of an n × n matrix (the lower triangle, column by column)."),
  mantel_noyau = c(
    "Le test de Mantel (vegan::mantel ; Mantel 1967) : la corrélation (de Pearson, ou de Spearman sur les rangs) entre les distances Dy et Dx des mêmes paires d'observations ; sa valeur p unilatérale, la part des permutations P des observations de Dy qui donnent une corrélation au moins aussi grande. Avec Dz, la corrélation partielle (vegan::mantel.partial ; Smouse et al. 1986), Dy permutée de même.",
    "The Mantel test (vegan::mantel; Mantel 1967): the correlation (Pearson's, or Spearman's on ranks) between the distances Dy and Dx of the same pairs of observations; its one-sided p-value, the share of the permutations P of the observations of Dy that give a correlation at least as large. With Dz, the partial correlation (vegan::mantel.partial; Smouse et al. 1986), Dy permuted the same way."),
  correlogramme_noyau = c(
    "Le corrélogramme de Mantel (vegan::mantel.correlog) : les paires d'observations en classes d'égale largeur de la distance Dx (`classes` = 0 : la règle de Sturges, 1 + log2 du nombre de paires, arrondi au-dessus) ; dans chaque classe, la corrélation de Mantel entre l'appartenance des paires à la classe (1 ou 0) et Dy, de signe changé (positive : des observations proches plus semblables que la moyenne), sa valeur p unilatérale du côté de son signe par les permutations P ; au-delà de la moitié des classes, une classe n'est testée que si chaque observation y a une paire. Les valeurs p corrigées progressivement par la méthode de Holm (la classe k avec les k premières).",
    "The Mantel correlogram (vegan::mantel.correlog): the pairs of observations in classes of equal width of the distance Dx (`classes` = 0: Sturges' rule, 1 + log2 of the number of pairs, rounded up); in each class, the Mantel correlation between the pairs' membership of the class (1 or 0) and Dy, its sign changed (positive: close observations more alike than average), its one-sided p-value on the side of its sign by the permutations P; beyond half of the classes, a class is tested only if every observation has a pair in it. The p-values corrected progressively by Holm's method (class k with the first k)."),
  rad = c("Les distances (en mètres) entre des points : le long d'un grand cercle (formule de haversine, sur le rayon moyen de WGS 84) pour des longitudes et latitudes en degrés (`lonlat`), en ligne droite pour des coordonnées planes.",
          "The distances (in metres) between points: along a great circle (haversine formula, on the WGS 84 mean radius) for longitudes and latitudes in degrees (`lonlat`), in straight lines for plane coordinates."),
  combinaisons_groupes = c(
    "Les combinaisons de groupes, de 1 à `ordre` groupes (indicspecies : cl.comb) : une colonne par combinaison (1 pour ses groupes), dans l'ordre de combn, nommée « a+b ».",
    "The combinations of groups, from 1 to `max_order` groups (indicspecies: cl.comb): one column per combination (1 for its groups), in combn's order, named “a+b”."),
  statistiques_indval = c(
    "A, B et la valeur indicatrice de chaque combinaison (lignes) pour chaque espèce (colonnes), comme indicspecies::multipatt : X, les abondances (observations × espèces) ; g, le numéro du groupe de chaque observation ; C, les combinaisons. A, la spécificité : la part de l'abondance de l'espèce dans la combinaison, d'après les moyennes des groupes (`egalises` : IndVal.g, De Cáceres et Legendre 2009) ou leurs sommes (IndVal, Dufrêne et Legendre 1997) ; B, la fidélité : la part des observations de la combinaison où l'espèce est présente ; la valeur indicatrice, √(A × B).",
    "A, B and the indicator value of each combination (rows) for each species (columns), as indicspecies::multipatt: X, the abundances (observations × species); g, the number of the group of each observation; C, the combinations. A, the specificity: the share of the species' abundance in the combination, from the means of the groups (`equalized`: IndVal.g, De Cáceres and Legendre 2009) or their sums (IndVal, Dufrêne and Legendre 1997); B, the fidelity: the share of the combination's observations where the species is present; the indicator value, √(A × B)."),
  meilleures_indval = c("La plus grande valeur de chaque espèce et sa combinaison (la première en cas d'égalité).",
                        "The largest value of each species and its combination (the first in a tie)."),
  test_indval = c(
    "Le test par permutation de cette plus grande valeur (De Cáceres et Legendre 2009) : pour chaque permutation (une ligne de P) des groupes des observations, la plus grande valeur de l'espèce recalculée ; p = (nombre de permutations où elle est au moins aussi grande + 1) / (permutations + 1).",
    "The permutation test of that largest value (De Cáceres and Legendre 2009): for each permutation (a row of P) of the observations' groups, the species' largest value computed again; p = (number of permutations where it is at least as large + 1) / (permutations + 1)."),
  EPS_PERMUTATIONS = c(
    "La valeur p d'un test par permutation : la part des permutations (et de l'observation elle-même) dont la statistique est au moins aussi grande ; une statistique permutée égale à l'observée aux erreurs d'arrondi près (EPS_PERMUTATIONS) compte.",
    "The p-value of a permutation test: the share of the permutations (and of the observation itself) whose statistic is at least as large; a permuted statistic equal to the observed one up to rounding errors (EPS_PERMUTATIONS) counts."))

# Le code des définitions `noms` du moteur, chacune précédée de son commentaire (DESCRIPTIONS_MOTEUR,
# dans la langue du script ; script_moteur.R : code_moteur garde, lui, ceux du moteur).
code_moteur_decrit <- function(noms, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  unlist(lapply(seq_along(noms), function(k) {
    d <- DESCRIPTIONS_MOTEUR[[noms[k]]]
    c(if (!is.null(d)) c(if (k > 1) "", commentaire(tx(d[1], d[2]))), code_definition_moteur(noms[k], langue))
  }))
}

# Ce que le script a de plus que le moteur : la graine des départs du NMDS, sans rendre ensuite le
# hasard de la séance (le moteur, lui, le rend : arbres.R, avec_graine).
CODE_AVEC_GRAINE <- r"---(
#fr Évalue `expr` après set.seed(graine), avec le générateur de R par défaut : les mêmes tirages que Nuée.
#en Evaluates `expr` after set.seed(seed), with R's default generator: the same draws as Nuée's.
avec_graine <- function(graine, expr) {
  set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  expr
}
)---"

# Les morceaux de texte (CODE_PERMUTER...) en lignes, chacun suivi d'une ligne vide.
lignes_code <- function(...) unlist(lapply(c(...), function(x) c(strsplit(sub("^\n", "", x), "\n", fixed = TRUE)[[1]], "")))

# ---------------------------------------------------------------- les fonctions de chaque analyse

# Les réglages d'un modèle (ceux des espèces indicatrices ont leur nom) et ce qui en découle pour le
# script : des permutations, des X ajustés sur une ordination, la distance géographique.
reglages_script <- function(m) {
  o <- if (identical(m$type, "INDVAL")) m$reglages_indval else m$options
  list(o = o, B = as.integer(o$permutations), ajustement = m$type %in% c("PCOA", "NMDS") && !is.null(m$ajustement),
       geographique = identical(m$type, "MANTEL") && identical(o$distance_x, "geographique"),
       z = identical(m$type, "MANTEL") && length(m$Z$termes) > 0)
}

code_fonctions_communaute <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  r <- reglages_script(m)
  o <- r$o
  type <- m$type
  if (type == "INDVAL") {
    return(c(code_moteur_decrit(c("combinaisons_groupes", "statistiques_indval", "meilleures_indval", if (r$B > 0) c("test_indval", "EPS_PERMUTATIONS")), langue), "",
             if (r$B > 0) lignes_code(CODE_PERMUTER)))
  }
  correction <- type %in% c("PCOA", "PERMANOVA") && !identical(o$correction, "aucune")
  distance <- code_moteur_decrit(c(if (!identical(o$transformation, "aucune")) "transformer_communaute", "ZERO_DISTANCE", "distance_tableau",
                            if (type %in% c("PCOA", "PERMANOVA")) "centrage_double", if (correction) "corriger_distance"), langue)
  analyse <- code_moteur_decrit(c(if (type == "PCOA") c("pcoa_noyau", "table_valeurs_pcoa"), if (type == "NMDS") c("procrustes_symetrique", "nmds_noyau"),
                           if (type %in% c("PCOA", "NMDS")) "scores_especes", if (r$ajustement) c("correlations_colonnes", "ajustement_environnement", "centroides_facteurs"),
                           if (type == "PERMANOVA") c("variables_groupe", "contenu_dans", "permanova_noyau", if (length(m$dispersions)) c("mediane_spatiale", "dispersion_noyau")),
                           if (type == "MANTEL") c("paires_triangle", "mantel_noyau", "correlogramme_noyau", if (r$geographique) c("rad", "RAYON_TERRE", "distances_entre_points"))),
                         langue)
  tests <- if (r$ajustement || type %in% c("PERMANOVA", "MANTEL")) c(if (r$B > 0) lignes_code(CODE_PERMUTER), code_moteur_decrit(c("EPS_PERMUTATIONS", "valeur_p_superieure"), langue), "")
  c(distance, "", analyse, "",
    if (type == "NMDS") lignes_code(CODE_AVEC_GRAINE),
    if (type == "MANTEL" && (!r$geographique || r$z)) c(code_echelle_terme(tx), ""),
    tests)
}

# ---------------------------------------------------------------- le calcul de ce modèle

# Les lignes du modèle dans le tableau du script : celles que sa condition choisit (condition.R),
# toutes, ou leurs rangs.
code_lignes_communaute <- function(projet, d, tab, tx) {
  obs <- as.numeric(d$observations)
  rangs <- unname(tab$rangs[as.character(obs)])
  pc <- plan_condition(projet, d, tab)
  if (!is.null(pc)) {
    return(c(code_condition(pc, tx, tx(sprintf("Les %d observations du modèle.", length(obs)), sprintf("The %d observations of the model.", length(obs)))), "d <- donnees[lignes, ]"))
  }
  if (identical(as.integer(rangs), seq_len(nrow(tab$donnees)))) {
    return(c(commentaire(tx(sprintf("Les %d observations du tableau.", length(obs)), sprintf("The %d observations of the table.", length(obs)))), "d <- donnees"))
  }
  c(commentaire(tx(sprintf("Les %d observations du modèle (leurs rangs dans le tableau).", length(obs)), sprintf("The %d observations of the model (their rows in the table).", length(obs)))),
    code_valeurs(morceaux_rangs(rangs), "lignes <- "), "d <- donnees[lignes, ]")
}

# Les observations que le modèle laisse de côté, dites dans un commentaire (quoi : pourquoi).
code_exclues <- function(projet, exclues, tx, quoi) {
  n <- length(exclues)
  if (!n) return(character(0))
  noms <- if (n <= 8) sprintf(" (%s)", liste_textes(texte_sur(unname(projet$noms_obs[as.character(exclues)])), tx("et", "and"))) else ""
  commentaire(tx(sprintf("Ici, %d observation%s%s : %s.", n, if (n > 1) "s" else "", noms, quoi[1]), sprintf("Here, %d observation%s%s: %s.", n, if (n > 1) "s" else "", noms, quoi[2])))
}

# Les groupes des permutations (un identifiant ou une variable : le site), ou NULL.
code_strates_communaute <- function(spec, tab, tx) {
  if (is.null(spec)) return(NULL)
  col <- if (identical(as.character(spec$by)[1], "id")) tab$ids[[as.character(spec$id)[1]]] else tab$noms[[as.character(spec$variable)[1]]]
  c(commentaire(tx(sprintf("Les groupes des permutations : « %s » (les observations ne sont permutées qu'à l'intérieur de chacun).", texte_sur(col)),
                   sprintf("The groups of the permutations: “%s” (the observations are only permuted within each).", texte_sur(col)))),
    sprintf("strates <- d[[%s]]", code_texte(col)))
}

# Le tableau transformé puis la distance entre les observations.
code_distance_communaute <- function(o, tx) {
  quoi <- switch(o$transformation,
    racine2 = tx("la racine carrée des valeurs", "the square root of the values"), racine4 = tx("leur racine quatrième", "their fourth root"),
    log1p = "log(x + 1)", presence = tx("les présences-absences", "presence–absence"),
    relative = tx("les abondances relatives (chaque observation divisée par son total)", "the relative abundances (each observation divided by its total)"),
    wisconsin = tx("la double standardisation de Wisconsin", "Wisconsin double standardization"),
    racine_wisconsin = tx("la racine carrée, puis la double standardisation de Wisconsin", "the square root, then Wisconsin double standardization"), NULL)
  c(if (!is.null(quoi)) c(commentaire(tx(sprintf("Le tableau transformé avant la distance : %s.", quoi), sprintf("The table transformed before the distance: %s.", quoi))),
                          sprintf("W <- transformer_communaute(Y, %s)", code_texte(o$transformation)))
    else c(commentaire(tx("Le tableau tel quel (sans transformation).", "The table as it is (no transformation).")), "W <- Y"),
    commentaire(tx(sprintf("La distance %s entre les observations.", nom_distance(o$distance, o$binaire, TRUE)),
                   sprintf("The %s distance between the observations.", nom_distance(o$distance, o$binaire, FALSE)))),
    sprintf("D <- distance_tableau(W, %s%s)", code_texte(o$distance), if (isTRUE(o$binaire)) ", binaire = TRUE" else ""))
}

# La correction de la distance (PCoA, PERMANOVA), s'il y en a une : `corr`, la distance corrigée
# (corr$D) et sa constante.
code_correction_communaute <- function(o, tx) {
  if (identical(o$correction, "aucune")) return(character(0))
  c(commentaire(if (identical(o$correction, "lingoes")) tx("La distance corrigée par la méthode de Lingoes (√(d² + 2c) ; Lingoes 1971) : sa PCoA n'a plus de valeur propre négative.",
                                                            "The distance corrected by the Lingoes method (√(d² + 2c); Lingoes 1971): its PCoA has no negative eigenvalue left.")
                else tx("La distance corrigée par la méthode de Cailliez (d + c ; Cailliez 1983) : sa PCoA n'a plus de valeur propre négative.",
                        "The distance corrected by the Cailliez method (d + c; Cailliez 1983): its PCoA has no negative eigenvalue left.")),
    sprintf("corr <- corriger_distance(D, %s)", code_texte(o$correction)))
}

# Les permutations des observations (graine de Nuée) : `n` observations, à l'intérieur des groupes
# `strates` (le code qui les donne) s'il y en a.
code_permutations_communaute <- function(B, graine, tx, n = "nrow(Y)", strates = NULL) {
  if (B == 0) {
    return(c(commentaire(tx("Sans permutation (aucun test) : une matrice de permutations vide.", "Without permutations (no test): an empty matrix of permutations.")),
             sprintf("permutations <- matrix(0L, 0, %s)", n)))
  }
  c(commentaire(tx(sprintf("Les %d permutations des observations%s (graine %d : les mêmes que Nuée).", B, if (!is.null(strates)) ", à l'intérieur de leurs groupes" else "", graine),
                   sprintf("The %d permutations of the observations%s (seed %d: the same as Nuée's).", B, if (!is.null(strates)) ", within their groups" else "", graine))),
    sprintf("permutations <- permuter(%s, %d%s, graine = %d)", n, B, if (!is.null(strates)) paste0(", ", strates) else "", graine))
}

# La partie « modèle » du script (dans la langue du script, ses noms en français :
# traduire_communautes les traduit).
code_calcul_communaute <- function(projet, d, m, tab, langue) {
  switch(m$type, PCOA = , NMDS = code_calcul_ordination_distance(projet, d, m, tab, langue), PERMANOVA = code_calcul_permanova(projet, d, m, tab, langue),
         MANTEL = code_calcul_mantel(projet, d, m, tab, langue), INDVAL = code_calcul_indval(projet, d, m, tab, langue))
}

explication_y <- function(tx) tx("Le tableau Y : une colonne par variable Y (les espèces, ou les variables dont on calcule la distance).",
                                 "The table Y: a column per Y variable (the species, or the variables whose distance is computed).")

# Une PCoA ou un NMDS, et les variables X ajustées sur l'ordination.
code_calcul_ordination_distance <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  r <- reglages_script(m)
  o <- r$o
  obs <- as.numeric(d$observations)
  nmds <- identical(m$type, "NMDS")
  objet <- if (nmds) "nmds" else "pcoa"
  quantitatifs <- Filter(function(t) categorie(t) == 0, d$x)
  qualitatives <- unique(as.character(unlist(lapply(d$x, function(t) if (categorie(t) != 0) t$variables[1]))))
  noms_q <- vapply(qualitatives, function(u) as.character(variable_du_projet(projet, u)$ids[1]), "", USE.NAMES = FALSE)
  strates <- if (r$ajustement) code_strates_communaute(o$strates, tab, tx)
  correction <- if (!nmds) code_correction_communaute(o, tx)
  Dp <- if (length(correction)) "corr$D" else "D"
  ajustement <- if (r$ajustement) {
    a <- m$ajustement
    axes <- if (nmds) tx(if (o$k > 1) sprintf("les %d dimensions du NMDS", o$k) else "la dimension du NMDS", if (o$k > 1) sprintf("the %d dimensions of the NMDS", o$k) else "the dimension of the NMDS")
            else tx("les deux premiers axes de la PCoA", "the first two axes of the PCoA")
    c("", commentaire(tx(sprintf("Les variables X ajustées sur %s (comme vegan::envfit), sur les observations dont les valeurs X%s sont connues : pour une variable quantitative, la direction de sa régression sur les axes et son R² ; pour une variable qualitative, les centroïdes de ses catégories et son R² ; leurs valeurs p par permutation de ces observations.",
                                 axes, if (!is.null(strates)) " et le groupe" else ""),
                         sprintf("The X variables fitted onto %s (as vegan::envfit), on the observations whose X values%s are known: for a quantitative variable, the direction of its regression on the axes and its R²; for a qualitative variable, the centroids of its categories and its R²; their p-values by permutations of these observations.",
                                 axes, if (!is.null(strates)) " and group" else ""))),
      code_exclues(projet, a$exclues, tx, c("une valeur X y manque", "an X value is missing")),
      sprintf("axes <- %s", if (nmds) "colnames(nmds$points)" else "colnames(pcoa$points)[seq_len(min(2, ncol(pcoa$points)))]"),
      sprintf("connues <- complete.cases(%s)", paste(c(if (length(quantitatifs)) "Xq", sprintf("facteurs[[%s]]", vapply(noms_q, code_texte, "", USE.NAMES = FALSE)),
                                                       if (!is.null(strates)) "strates"), collapse = ", ")),
      code_permutations_communaute(r$B, o$graine, tx, "sum(connues)", if (!is.null(strates)) "strates[connues]"),
      "facteurs_connus <- lapply(facteurs, function(f) f[connues])",
      sprintf("ajustement <- ajustement_environnement(%s$points[connues, axes, drop = FALSE], Xq[connues, , drop = FALSE], facteurs_connus, permutations)", objet),
      sprintf("centroides <- centroides_facteurs(%s$points[connues, , drop = FALSE], facteurs_connus)", objet),
      if (length(quantitatifs)) c("v <- ajustement$vecteurs",
                                  "variables_ajustees <- data.frame(variable = names(v$r2), R2 = unname(v$r2), p = unname(v$p), v$direction, row.names = NULL, check.names = FALSE)"),
      if (length(qualitatives)) c("facteurs_ajustes <- data.frame(variable = vapply(ajustement$facteurs, function(f) f$variable, \"\"), R2 = vapply(ajustement$facteurs, function(f) f$r2, 0),",
                                  "                               p = vapply(ajustement$facteurs, function(f) f$p, 0))"))
  }
  c(code_lignes_communaute(projet, d, tab, tx), "",
    code_bloc_ordination(projet, d$y, obs, tab, "Y", langue, explication_y(tx)), "",
    if (r$ajustement) c(
      if (length(quantitatifs)) code_bloc_ordination(projet, quantitatifs, obs, tab, "Xq", langue, tx("Les variables X quantitatives, à ajuster sur l'ordination : leurs valeurs dans leurs unités.",
                                                                                                 "The quantitative X variables, to fit onto the ordination: their values in their units."))
      else c(commentaire(tx("(aucune variable X quantitative)", "(no quantitative X variable)")), "Xq <- matrix(numeric(0), nrow(d), 0)"),
      if (length(qualitatives)) c(commentaire(tx("Les variables X qualitatives : leurs catégories (un centroïde par catégorie).", "The qualitative X variables: their categories (a centroid per category).")),
                                  code_valeurs(sprintf("%s = d[[%s]]", vapply(noms_q, nom_element, "", USE.NAMES = FALSE), vapply(qualitatives, function(u) code_texte(tab$noms[[u]]), "", USE.NAMES = FALSE)),
                                               "facteurs <- list(", ")", ouvrir = ""))
      else "facteurs <- list()",
      strates, ""),
    commentaire(tx("Les observations où une valeur Y manque sont laissées de côté.", "The observations where a Y value is missing are left out.")),
    code_exclues(projet, m$exclues, tx, c("une valeur Y y manque", "a Y value is missing")),
    "complet <- complete.cases(Y)",
    "Y <- Y[complet, , drop = FALSE]",
    "rownames(Y) <- d[[1]][complet]",
    if (r$ajustement) c("Xq <- Xq[complet, , drop = FALSE]", "facteurs <- lapply(facteurs, function(f) f[complet])", if (!is.null(strates)) "strates <- strates[complet]"),
    "",
    code_distance_communaute(o, tx), correction, "",
    if (!nmds) c(
      commentaire(tx("La PCoA, la part de l'inertie totale de chaque axe, et les scores des espèces (Y) sur ses axes positifs.",
                     "The PCoA, the share of the total inertia of each axis, and the scores of the species (Y) on its positive axes.")),
      sprintf("pcoa <- pcoa_noyau(%s)", Dp),
      "valeurs_propres <- table_valeurs_pcoa(pcoa)",
      "especes <- scores_especes(pcoa$points, W)")
    else c(
      commentaire(tx(sprintf("Le NMDS en %d dimension%s : depuis la configuration de la PCoA, puis %d départs au hasard (graine %d : les mêmes que Nuée) ; la configuration de plus faible stress, et les scores des espèces (Y).",
                             o$k, if (o$k > 1) "s" else "", o$departs, o$graine),
                     sprintf("The NMDS in %d dimension%s: from the configuration of the PCoA, then %d random starts (seed %d: the same as Nuée's); the configuration of lowest stress, and the scores of the species (Y).",
                             o$k, if (o$k > 1) "s" else "", o$departs, o$graine))),
      sprintf("nmds <- nmds_noyau(D, %d, %d, %d)", o$k, o$departs, o$graine),
      "especes <- scores_especes(nmds$points, W)"),
    ajustement)
}

# Une PERMANOVA, et la dispersion multivariée de ses groupements.
code_calcul_permanova <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  o <- m$options
  obs <- as.numeric(d$observations)
  strates <- code_strates_communaute(o$strates, tab, tx)
  correction <- code_correction_communaute(o, tx)
  Dp <- if (length(correction)) "corr$D" else "D"
  # les groupements de la dispersion, comme ajuster_permanova les fait (les variables X qualitatives,
  # puis la variable de classe), ceux que le modèle a gardés (deux groupes au moins), et leur colonne
  uids <- list()
  for (u in c(unlist(lapply(d$x, function(t) if (length(t$variables) == 1 && categorie(t) != 0) t$variables[1])), if (length(d$classe) && !is.na(d$classe[1])) d$classe[1])) {
    nom <- as.character(variable_du_projet(projet, u)$ids[1])
    if (is.null(uids[[nom]])) uids[[nom]] <- u
  }
  noms_g <- intersect(names(uids), names(m$dispersions))
  colonnes_g <- vapply(noms_g, function(nom) tab$noms[[as.character(uids[[nom]])]], "", USE.NAMES = FALSE)
  facon <- switch(o$par,
    termes = tx("chaque terme après ceux qui le précèdent (sommes des carrés séquentielles)", "each term after those before it (sequential sums of squares)"),
    marges = tx("chaque terme après tous les autres (effets marginaux)", "each term after all the others (marginal effects)"),
    modele = tx("le modèle entier", "the whole model"))
  centre <- if (identical(o$dispersion, "mediane")) tx("à la médiane spatiale", "to the spatial median") else tx("au centroïde", "to the centroid")
  c(code_lignes_communaute(projet, d, tab, tx), "",
    code_bloc_ordination(projet, d$y, obs, tab, "Y", langue, explication_y(tx)), "",
    code_bloc_ordination(projet, d$x, obs, tab, "X", langue, tx("Les termes X : leurs valeurs dans leurs unités (une indicatrice 0/1 par catégorie d'une variable qualitative).",
                                                              "The X terms: their values in their units (a 0/1 indicator for each category of a qualitative variable).")),
    commentaire(tx("Le terme de chaque colonne de X (une variable, toutes ses catégories ensemble ; un terme croisé, « A*B »), dans l'ordre des termes.",
                   "The term of each column of X (a variable, all its categories together; a cross term, “A*B”), in the order of the terms.")),
    code_valeurs(vapply(m$X$groupes, code_texte, "", USE.NAMES = FALSE), "termes_x <- "), "",
    if (!is.null(strates)) c(strates, ""),
    if (length(noms_g)) c(commentaire(tx("Les groupements de la dispersion multivariée : les variables X qualitatives, puis la variable de classe (rôle Classe).",
                                         "The groupings of the multivariate dispersion: the qualitative X variables, then the class variable (Class role).")),
                          code_valeurs(sprintf("%s = d[[%s]]", vapply(noms_g, nom_element, "", USE.NAMES = FALSE), vapply(colonnes_g, code_texte, "", USE.NAMES = FALSE)),
                                       "groupements <- list(", ")", ouvrir = ""), ""),
    commentaire(tx(sprintf("Les observations où une valeur Y ou X manque%s sont laissées de côté.", if (!is.null(strates)) ", ou leur groupe," else ""),
                   sprintf("The observations where a Y or X value is missing%s are left out.", if (!is.null(strates)) ", or their group," else ""))),
    code_exclues(projet, m$exclues, tx, c("une valeur y manque", "a value is missing")),
    sprintf("complet <- complete.cases(Y, X%s)", if (!is.null(strates)) ", strates" else ""),
    "Y <- Y[complet, , drop = FALSE]",
    "rownames(Y) <- d[[1]][complet]",
    "X <- X[complet, , drop = FALSE]",
    if (!is.null(strates)) "strates <- strates[complet]",
    if (length(noms_g)) "groupements <- lapply(groupements, function(f) f[complet])",
    "",
    code_distance_communaute(o, tx), correction, "",
    code_permutations_communaute(o$permutations, o$graine, tx, "nrow(Y)", if (!is.null(strates)) "strates"), "",
    commentaire(tx(sprintf("La PERMANOVA (comme vegan::adonis2) de G = −½ D² doublement centrée sur les colonnes de X : %s, son pseudo-F comparé à celui des permutations.", facon),
                   sprintf("The PERMANOVA (as vegan::adonis2) of G = −½ D² doubly centred on the columns of X: %s, its pseudo-F compared with that of the permutations.", facon))),
    sprintf("G <- -0.5 * centrage_double(%s^2)", Dp),
    sprintf("permanova <- permanova_noyau(G, X, termes_x, %s, permutations)", code_texte(o$par)),
    if (length(noms_g)) c("",
      commentaire(tx(sprintf("La dispersion multivariée de chaque groupement (comme vegan::betadisper) : la distance de chaque observation %s de son groupe, sur les observations dont le groupe est connu ; son ANOVA, ses résidus permutés (les mêmes permutations quand toutes le sont), et les paires de Tukey.", centre),
                     sprintf("The multivariate dispersion of each grouping (as vegan::betadisper): the distance of each observation %s of its group, on the observations whose group is known; its ANOVA, its residuals permuted (the same permutations when all are known), and Tukey's pairs.", centre))),
      "dispersion <- function(groupe) {",
      "  connus <- !is.na(groupe)",
      if (o$permutations > 0) sprintf("  P <- if (all(connus)) permutations else permuter(sum(connus), %d%s, graine = %d)", o$permutations, if (!is.null(strates)) ", strates[connus]" else "", o$graine)
      else "  P <- matrix(0L, 0, sum(connus))",
      sprintf("  dispersion_noyau(%s[connus, connus, drop = FALSE], groupe[connus], %s, P)", Dp, code_texte(o$dispersion)),
      "}",
      "dispersions <- lapply(groupements, dispersion)"))
}

# Les termes d'un test de Mantel (X ou Z) : leurs valeurs, puis leur mise à l'échelle comme dans
# Nuée (construire_bloc : l'échelle de chaque terme, sa pondération ; un terme croisé, le produit de
# ses facteurs mis à l'échelle, mis à l'échelle à son tour), sur les observations du test.
plan_termes_mantel <- function(projet, termes, obs, tab, langue, M, explication) {
  tx <- function(fr, en) if (langue == "en") en else fr
  types <- vapply(termes, function(t) as.character(t$scaling$type %||% "UV")[1], "")
  if (!all(types %in% c("UV", "Par", "Ctr", "none"))) {
    stop(traduire("Export vers R : la mise à l'échelle de ces termes (enregistrée dans un fichier) n'est pas encore écrite dans le script.",
                  "Export to R: the scaling of these terms (stored in a file) isn't written in the script yet."), call. = FALSE)
  }
  ponderations <- facteurs_ponderation(termes)
  simples <- which(vapply(termes, function(t) length(t$variables) == 1, TRUE))
  croises <- setdiff(seq_along(termes), simples)
  noms <- vapply(termes, function(t) as.character(t$name), "")
  m <- tolower(M)
  code_croises <- vapply(croises, function(k) {
    t <- termes[[k]]
    facteurs <- vapply(seq_along(t$variables), function(j) {
      s <- simples[vapply(termes[simples], function(x) identical(as.numeric(x$variables), as.numeric(t$variables[j])) && categorie(x) == categorie(t, j), TRUE)][1]
      sprintf("%s[, %s]", M, code_texte(noms[s]))
    }, "")
    produit <- paste(facteurs, collapse = " * ")
    if (!is.null(t$transform)) produit <- code_transformation(t$transform, sprintf("(%s)", produit), TRUE)  # (sa transformation, avant l'échelle)
    sprintf("%s <- cbind(%s, %s = echelle_terme(%s, %s, %s))", M, M, code_texte(noms[k]), produit, code_texte(types[k]), texte_exact(ponderations[k]))
  }, "")
  list(lecture = code_bloc_ordination(projet, termes_bruts(termes[simples]), obs, tab, M, langue, explication),
       echelle = c(code_valeurs(vapply(types[simples], code_texte, "", USE.NAMES = FALSE), sprintf("echelles_%s <- ", m)),
                   code_valeurs(texte_exact(ponderations[simples]), sprintf("ponderations_%s <- ", m)),
                   sprintf("for (j in seq_len(ncol(%s))) %s[, j] <- echelle_terme(%s[, j], echelles_%s[j], ponderations_%s[j])", M, M, M, m, m),
                   if (length(croises)) c(commentaire(tx("Les termes croisés : le produit de leurs facteurs mis à l'échelle, mis à l'échelle à son tour.",
                                                         "The cross terms: the product of their scaled factors, scaled in turn.")), code_croises)))
}

# Un test de Mantel (simple ou partiel), sur la distance des X ou la distance géographique, et son
# corrélogramme.
code_calcul_mantel <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  r <- reglages_script(m)
  o <- r$o
  obs <- as.numeric(d$observations)
  strates <- code_strates_communaute(o$strates, tab, tx)
  x <- if (!r$geographique) plan_termes_mantel(projet, d$x, obs, tab, langue, "X",
                                               tx("Les termes X, par leurs valeurs (une indicatrice 0/1 par catégorie d'une variable qualitative) ; mis à l'échelle plus bas, sur les observations du test.",
                                                  "The X terms, by their values (a 0/1 indicator for each category of a qualitative variable); scaled below, on the observations of the test."))
  z <- if (r$z) plan_termes_mantel(projet, termes_z_mantel(projet, o), obs, tab, langue, "Z",
                                   tx("Les covariables Z du test partiel, par leurs valeurs ; mises à l'échelle plus bas.", "The covariates Z of the partial test, by their values; scaled below."))
  geo <- if (r$geographique) {
    co <- coordonnees_projet(projet, exiger = TRUE)
    s <- systeme_coordonnees(co$systeme)
    list(x = tab$noms[[as.character(co$x)]], y = tab$noms[[as.character(co$y)]], nom = texte_sur(s$nom), lonlat = identical(s$type, "lonlat"))
  }
  methode <- if (identical(o$methode, "spearman")) tx("de Spearman (sur les rangs)", "Spearman's (on ranks)") else tx("de Pearson", "Pearson's")
  manque <- c(if (!r$geographique) "X", if (r$z) "Z")
  c(code_lignes_communaute(projet, d, tab, tx), "",
    code_bloc_ordination(projet, d$y, obs, tab, "Y", langue, explication_y(tx)), "",
    if (!r$geographique) c(x$lecture, "")
    else c(commentaire(tx(sprintf("Les coordonnées des observations : « %s » et « %s » (%s).", texte_sur(geo$x), texte_sur(geo$y), geo$nom),
                          sprintf("The coordinates of the observations: “%s” and “%s” (%s).", texte_sur(geo$x), texte_sur(geo$y), geo$nom))),
           sprintf("coordonnees <- cbind(d[[%s]], d[[%s]])", code_texte(geo$x), code_texte(geo$y)), ""),
    if (r$z) c(z$lecture, ""),
    if (!is.null(strates)) c(strates, ""),
    commentaire(tx(sprintf("Les observations où une valeur %s manque%s%s sont laissées de côté.", liste_textes(c("Y", manque), "ou"),
                           if (r$geographique) ", ou leurs coordonnées," else "", if (!is.null(strates)) ", ou leur groupe," else ""),
                   sprintf("The observations where a %s value is missing%s%s are left out.", liste_textes(c("Y", manque), "or"),
                           if (r$geographique) ", or their coordinates," else "", if (!is.null(strates)) ", or their group," else ""))),
    code_exclues(projet, m$exclues, tx, c("une valeur y manque", "a value is missing")),
    sprintf("complet <- complete.cases(Y%s%s%s)%s", if (!r$geographique) ", X" else "", if (r$z) ", Z" else "", if (!is.null(strates)) ", strates" else "",
            if (r$geographique) " & is.finite(coordonnees[, 1]) & is.finite(coordonnees[, 2])" else ""),
    "Y <- Y[complet, , drop = FALSE]",
    "rownames(Y) <- d[[1]][complet]",
    if (!r$geographique) "X <- X[complet, , drop = FALSE]" else "coordonnees <- coordonnees[complet, , drop = FALSE]",
    if (r$z) "Z <- Z[complet, , drop = FALSE]",
    if (!is.null(strates)) "strates <- strates[complet]",
    "",
    if (!r$geographique) c(
      commentaire(tx("La distance des X : chaque terme mis à l'échelle comme dans Nuée (onglet Variables : son échelle, UV par défaut ; sa pondération, bloc et modificateur), sur ces observations, puis la distance euclidienne entre elles.",
                     "The distance of the X: each term scaled as in Nuée (Variables tab: its scaling, UV by default; its weighting, block and modifier), on these observations, then the Euclidean distance between them.")),
      x$echelle, "Dx <- as.matrix(dist(X))")
    else c(
      commentaire(if (geo$lonlat) tx("La distance géographique en kilomètres entre les observations, le long des grands cercles (formule de haversine).",
                                     "The geographic distance in kilometres between the observations, along great circles (haversine formula).")
                  else tx("La distance géographique en kilomètres entre les observations, en ligne droite.", "The geographic distance in kilometres between the observations, in straight lines.")),
      sprintf("Dx <- distances_entre_points(coordonnees[, 1], coordonnees[, 2], lonlat = %s) / 1000", if (geo$lonlat) "TRUE" else "FALSE")),
    if (r$z) c("", commentaire(tx("La distance des covariables Z : chacune mise à l'échelle (UV par défaut), puis la distance euclidienne.", "The distance of the covariates Z: each scaled (UV by default), then the Euclidean distance.")),
               z$echelle, "Dz <- as.matrix(dist(Z))"),
    "",
    code_distance_communaute(o, tx), "",
    code_permutations_communaute(o$permutations, o$graine, tx, "nrow(Y)", if (!is.null(strates)) "strates"), "",
    commentaire(tx(sprintf("Le test de Mantel : la corrélation %s entre les distances des mêmes paires d'observations, la distance Y permutée%s.", methode, if (r$z) " ; partiel, sans la distance des Z" else ""),
                   sprintf("The Mantel test: %s correlation between the distances of the same pairs of observations, the Y distance permuted%s.", methode, if (r$z) "; partial, without the distance of the Z" else ""))),
    sprintf("mantel <- mantel_noyau(D, Dx, %s, permutations%s)", code_texte(o$methode), if (r$z) ", Dz" else ""),
    commentaire(tx(sprintf("Le corrélogramme de Mantel : la distance X en classes d'égale largeur (%s), les mêmes permutations.", if (o$classes > 0) sprintf("%d classes", as.integer(o$classes)) else "règle de Sturges"),
                   sprintf("The Mantel correlogram: the X distance in classes of equal width (%s), the same permutations.", if (o$classes > 0) sprintf("%d classes", as.integer(o$classes)) else "Sturges' rule"))),
    sprintf("correlogramme <- correlogramme_noyau(D, Dx, %s, permutations, %d)", code_texte(o$methode), as.integer(o$classes)))
}

# Les espèces indicatrices.
code_calcul_indval <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  o <- m$reglages_indval
  obs <- as.numeric(d$observations)
  col <- tab$noms[[as.character(d$classe)]]
  strates <- code_strates_communaute(o$strates, tab, tx)
  egalises <- identical(o$indice, "IndVal.g")
  k <- length(m$niveaux)
  combinaisons <- if (m$ordre <= 1) tx("les groupes seuls", "the single groups")
                  else if (m$ordre >= k) tx("toutes, d'un groupe à tous", "all of them, from one group to all")
                  else tx(sprintf("d'un à %d groupes", m$ordre), sprintf("from one to %d groups", m$ordre))
  c(code_lignes_communaute(projet, d, tab, tx), "",
    code_bloc_ordination(projet, d$y, obs, tab, "Y", langue, tx("Les espèces (rôle Y) : une colonne par espèce, leurs abondances ou leurs présences.",
                                                              "The species (Y role): a column per species, their abundances or presences.")), "",
    commentaire(tx(sprintf("Les groupes d'observations : « %s » (rôle Classe).", texte_sur(col)), sprintf("The groups of observations: “%s” (Class role).", texte_sur(col)))),
    sprintf("groupe <- d[[%s]]", code_texte(col)),
    if (!is.null(strates)) strates, "",
    commentaire(tx(sprintf("Les observations sans groupe, ou où une valeur manque%s, sont laissées de côté.", if (!is.null(strates)) ", ou sans groupe de permutation" else ""),
                   sprintf("The observations without a group, or where a value is missing%s, are left out.", if (!is.null(strates)) ", or without a permutation group" else ""))),
    code_exclues(projet, m$exclues, tx, c("sans groupe, ou une valeur y manque", "no group, or a value is missing")),
    sprintf("connu <- !is.na(groupe) & complete.cases(Y%s)", if (!is.null(strates)) ", strates" else ""),
    "Y <- Y[connu, , drop = FALSE]",
    "groupe <- groupe[connu]",
    if (!is.null(strates)) "strates <- strates[connu]",
    if (isTRUE(o$presence)) c(commentaire(tx("Les présences seules : 1 pour une valeur positive, 0 sinon.", "Presences only: 1 for a positive value, 0 otherwise.")), "Y[] <- (Y > 0) * 1"),
    commentaire(tx("Les groupes présents, dans l'ordre de leurs catégories, et le numéro du groupe de chaque observation.",
                   "The groups present, in the order of their categories, and the number of the group of each observation.")),
    "niveaux <- intersect(levels(groupe), as.character(groupe))",
    "g <- match(as.character(groupe), niveaux)",
    commentaire(tx("Les espèces absentes de toutes ces observations sont laissées de côté.", "The species absent from all these observations are left out.")),
    "absentes <- colnames(Y)[colSums(Y) <= 0]",
    "Y <- Y[, colSums(Y) > 0, drop = FALSE]",
    "",
    commentaire(tx(sprintf("Les combinaisons de groupes, %s ; pour chaque espèce et chaque combinaison, A, B et la valeur indicatrice %s ; la combinaison de sa plus grande valeur.",
                           combinaisons, if (egalises) "IndVal.g" else "IndVal"),
                   sprintf("The combinations of groups, %s; for each species and each combination, A, B and the indicator value %s; the combination of its largest value.",
                           combinaisons, if (egalises) "IndVal.g" else "IndVal"))),
    sprintf("C <- combinaisons_groupes(niveaux, %d)", as.integer(m$ordre)),
    sprintf("statistiques <- statistiques_indval(Y, g, C, egalises = %s)", if (egalises) "TRUE" else "FALSE"),
    "meilleures <- meilleures_indval(statistiques$IndVal)",
    "",
    if (o$permutations > 0) c(
      code_permutations_communaute(o$permutations, o$graine, tx, "nrow(Y)", if (!is.null(strates)) "strates"),
      commentaire(tx(sprintf("Le test de la plus grande valeur de chaque espèce, par ces permutations des groupes ; une espèce rattachée à tous les groupes n'est pas testée. Puis les valeurs p corrigées sur les espèces testées (%s).",
                             if (identical(o$correction, "holm")) "Holm" else "Benjamini–Hochberg"),
                     sprintf("The test of each species' largest value, by these permutations of the groups; a species tied to all the groups isn't tested. Then the p-values corrected over the species tested (%s).",
                             if (identical(o$correction, "holm")) "Holm" else "Benjamini–Hochberg"))),
      sprintf("p <- test_indval(Y, g, C, %s, meilleures$valeur, permutations)", if (egalises) "TRUE" else "FALSE"),
      "p[(colSums(C) == length(niveaux))[meilleures$rang]] <- NA",
      "p_ajuste <- rep(NA_real_, length(p))",
      sprintf("p_ajuste[!is.na(p)] <- p.adjust(p[!is.na(p)], %s)", code_texte(o$correction)))
    else c(commentaire(tx("(sans permutation, pas de test)", "(without permutations, no test)")), "p <- p_ajuste <- rep(NA_real_, ncol(Y))"),
    "",
    commentaire(tx("Le tableau des espèces : le groupe (ou la combinaison) de chacune, A, B, sa valeur indicatrice, p et p corrigée.",
                   "The table of species: the group (or combination) of each, A, B, its indicator value, p and corrected p.")),
    "j <- meilleures$rang",
    "especes <- data.frame(espece = colnames(Y), groupe = colnames(C)[j], A = statistiques$A[cbind(j, seq_along(j))], B = statistiques$B[cbind(j, seq_along(j))],",
    "                      indval = meilleures$valeur, p = p, p_ajuste = p_ajuste, row.names = NULL)")
}

# ---------------------------------------------------------------- les résultats affichés

code_sortie_communaute <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  ligne <- function(code, fr, en) {
    code <- traduire_communautes(code, langue)
    paste0(formatC(code, width = -max(34, nchar(code) + 2)), "# ", tx(fr, en))
  }
  r <- reglages_script(m)
  o <- r$o
  a <- m$ajustement
  ordination <- c(
    if (!is.null(m$especes)) ligne("print(especes[, seq_len(min(2, ncol(especes))), drop = FALSE])", "scores des espèces (Y), deux premiers axes", "scores of the species (Y), first two axes"),
    if (!is.null(a$vecteurs)) ligne("print(variables_ajustees)", "variables X quantitatives ajustées : R², p, direction", "fitted quantitative X variables: R², p, direction"),
    if (!is.null(a$facteurs)) c(ligne("print(facteurs_ajustes)", "variables X qualitatives ajustées : R², p", "fitted qualitative X variables: R², p"),
                                ligne("print(centroides[, axes, drop = FALSE])", "centroïdes de leurs catégories", "centroids of their categories")))
  switch(m$type,
    PCOA = c(ligne("print(valeurs_propres)", "valeurs propres, part de l'inertie totale, cumul (les négatives marquées)", "eigenvalues, share of the total inertia, cumulative (negative ones marked)"),
             if (!identical(o$correction, "aucune")) ligne("print(corr$constante)", "constante de la correction", "constant of the correction"),
             ligne("print(head(pcoa$points))", "coordonnées des premières observations (axes positifs)", "coordinates of the first observations (positive axes)"),
             ordination),
    NMDS = c(ligne("print(nmds$stress)", "stress de la configuration retenue (de 0 à 1)", "stress of the configuration kept (from 0 to 1)"),
             ligne("print(c(meilleur = nmds$meilleur, retrouvee = nmds$retrouvee))", "son départ (0 : la PCoA), et combien d'autres l'ont retrouvée", "its start (0: the PCoA), and how many others found it again"),
             ligne("print(nmds$stress_departs)", "stress de chaque départ", "stress of each start"),
             ligne("print(head(nmds$points))", "coordonnées des premières observations", "coordinates of the first observations"),
             ordination),
    PERMANOVA = c(ligne("print(permanova$table)", "termes, résidus et total : ddl, somme des carrés, R², pseudo-F, p", "terms, residuals and total: df, sum of squares, R², pseudo-F, p"),
                  if (length(m$noyau$alias)) ligne("print(permanova$alias)", "termes redondants avec les autres, laissés de côté", "terms redundant with the others, left out"),
                  if (!identical(o$correction, "aucune")) ligne("print(corr$constante)", "constante de la correction", "constant of the correction"),
                  unlist(lapply(names(m$dispersions), function(nom) {
                    q <- sprintf("dispersions[[%s]]", code_texte(nom))
                    c(ligne(sprintf("print(%s$groupes)", q), sprintf("dispersion de « %s » : distances moyenne et médiane au centre de chaque groupe", texte_sur(nom)),
                            sprintf("dispersion of “%s”: mean and median distances to the centre of each group", texte_sur(nom))),
                      ligne(sprintf("print(%s$anova)", q), "son ANOVA, et sa valeur p par permutation", "its ANOVA, and its permutation p-value"),
                      ligne(sprintf("print(%s$tukey)", q), "ses paires de groupes (Tukey)", "its pairs of groups (Tukey)"))
                  }))),
    MANTEL = c(ligne("print(unlist(mantel))", if (r$z) "r, p, r partiel, p partiel, permutations" else "r de Mantel, p, permutations",
                     if (r$z) "r, p, partial r, partial p, permutations" else "Mantel r, p, permutations"),
               if (!is.null(m$correlogramme)) ligne("print(correlogramme$table)", "corrélogramme : chaque classe, son centre, ses paires, r, p, p corrigée (Holm)",
                                                    "correlogram: each class, its centre, its pairs, r, p, corrected p (Holm)")),
    INDVAL = c(ligne("print(especes)", "chaque espèce : son groupe ou sa combinaison, A, B, valeur indicatrice, p, p corrigée",
                     "each species: its group or combination, A, B, indicator value, p, corrected p"),
               if (length(m$absentes)) ligne("print(absentes)", "espèces absentes de ces observations, laissées de côté", "species absent from these observations, left out"),
               if (o$permutations > 0) ligne("print(table(factor(especes$groupe, colnames(C))[!is.na(especes$p_ajuste) & especes$p_ajuste <= 0.05]))",
                                             "espèces indicatrices de chaque groupe (p corrigée ≤ 0,05)", "indicator species of each group (corrected p ≤ 0.05)"),
               ligne("print(round(t(statistiques$IndVal), 3))", "valeur indicatrice de chaque espèce pour chaque combinaison", "indicator value of each species for each combination")))
}

# Le nom d'une distance dans un titre de graphique : « Bray-Curtis », « euclidienne ».
nom_distance_titre <- function(o, langue) sub("^de ", "", nom_distance(o$distance, o$binaire, fr = !identical(langue, "en")))

# Les graphiques en R de base.
code_graphiques_communaute <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  t <- function(code) traduire_communautes(code, langue)
  r <- reglages_script(m)
  o <- r$o
  if (m$type %in% c("PCOA", "NMDS")) {
    nmds <- identical(m$type, "NMDS")
    objet <- if (nmds) "nmds" else "pcoa"
    a <- m$ajustement
    especes <- !is.null(m$especes)
    titre <- sprintf("%s (%s)", if (nmds) "NMDS" else "PCoA", nom_distance_titre(o, langue))
    ordination <- if (ncol(m$noyau$points) >= 2) c(
      if (!nmds) "etiquette_axe <- function(k) sprintf(\"%s (%.1f %%)\", valeurs_propres$axe[k], 100 * valeurs_propres$part[k])",
      sprintf("S <- %s$points[, 1:2]", objet),
      sprintf("plot(rbind(S%s), type = \"n\", asp = 1, xlab = %s, ylab = %s, main = %s)", if (especes) ", especes[, 1:2]" else "",
              if (nmds) "\"NMDS1\"" else "etiquette_axe(1)", if (nmds) "\"NMDS2\"" else "etiquette_axe(2)", code_texte(titre)),
      "abline(h = 0, v = 0, col = \"grey\")",
      "points(S, pch = 19, col = \"grey40\")",
      if (especes) "text(especes[, 1:2], labels = rownames(especes), col = \"#d1495b\", cex = 0.8)",
      if (!is.null(a$vecteurs)) c(
        commentaire(tx("Les variables X quantitatives : leurs flèches (direction × √R²), agrandies pour remplir le graphique ; pâles quand p > 0,05.",
                       "The quantitative X variables: their arrows (direction × √R²), enlarged to fill the plot; pale when p > 0.05.")),
        "fleches <- ajustement$vecteurs$direction[, 1:2, drop = FALSE] * sqrt(ajustement$vecteurs$r2)",
        "limites <- par(\"usr\")",
        "multiplicateur <- 0.75 * min(abs(limites[1:2]) / max(abs(fleches[, 1])), abs(limites[3:4]) / max(abs(fleches[, 2])))",
        "couleurs <- ifelse(is.na(ajustement$vecteurs$p) | ajustement$vecteurs$p <= 0.05, \"#2f6fb5\", \"#9aa7b8\")",
        "arrows(0, 0, multiplicateur * fleches[, 1], multiplicateur * fleches[, 2], length = 0.08, col = couleurs)",
        "text(1.08 * multiplicateur * fleches, labels = rownames(fleches), col = couleurs, cex = 0.85)"),
      if (!is.null(a$facteurs)) c("points(centroides[, 1:2, drop = FALSE], pch = 17, col = \"#2f6fb5\")",
                                  "text(centroides[, 1:2, drop = FALSE], labels = rownames(centroides), col = \"#2f6fb5\", cex = 0.8, pos = 3)"))
    else sprintf("stripchart(%s$points[, 1], pch = 19, col = \"grey40\", xlab = %s, main = %s)", objet, code_texte(colnames(m$noyau$points)[1]), code_texte(titre))
    autre <- if (!nmds) {
      list(nom = "valeurs-propres", largeur = 7, hauteur = 4, code = t(c(
        "barplot(100 * valeurs_propres$part, names.arg = valeurs_propres$axe, las = 2, border = NA, col = ifelse(valeurs_propres$negative, \"#d1495b\", \"#2f6fb5\"),",
        sprintf("        ylab = %s, main = %s)", code_texte(tx("% de l'inertie totale", "% of the total inertia")), code_texte(tx("Valeurs propres (en rouge : négatives)", "Eigenvalues (in red: negative)"))))))
    } else {
      list(nom = "shepard", largeur = 6, hauteur = 5, code = t(c(
        "plot(nmds$shepard$dissimilarite, nmds$shepard$distance, pch = 20, cex = 0.6, col = \"#2f6fb580\",",
        sprintf("     xlab = %s, ylab = %s, main = sprintf(%s, nmds$stress))", code_texte(tx("dissimilarité", "dissimilarity")), code_texte(tx("distance dans l'ordination", "distance in the ordination")),
                code_texte(tx("Diagramme de Shepard (stress %.3f)", "Shepard diagram (stress %.3f)"))),
        "lines(nmds$shepard$dissimilarite, nmds$shepard$ajustee, type = \"s\", col = \"#d1495b\", lwd = 2)")))
    }
    return(list(list(nom = "ordination", largeur = 7, hauteur = 6, code = t(ordination)), autre))
  }
  if (m$type == "PERMANOVA") {
    nd <- length(m$dispersions)
    return(c(list(list(nom = "permanova", largeur = 6, hauteur = 4, code = t(c(
        "termes_table <- permanova$table[seq_len(nrow(permanova$table) - 2), , drop = FALSE]",
        "barplot(100 * termes_table$R2, names.arg = termes_table$terme, las = 2, border = NA,",
        "        col = ifelse(!is.na(termes_table$p) & termes_table$p <= 0.05, \"#2f6fb5\", \"#9aa7b8\"),",
        sprintf("        ylab = %s, main = %s)", code_texte(tx("% de la somme des carrés (R²)", "% of the sum of squares (R²)")),
                code_texte(tx("PERMANOVA (en bleu : p ≤ 0,05)", "PERMANOVA (in blue: p ≤ 0.05)"))))))),
      if (nd) list(list(nom = "dispersion", largeur = min(4 * nd, 12), hauteur = 4, code = t(c(
        sprintf("op <- par(mfrow = c(1, %d))", nd),
        "for (groupement in names(dispersions)) {",
        "  x <- dispersions[[groupement]]",
        sprintf("  boxplot(x$distances ~ x$groupe, xlab = groupement, ylab = %s, col = \"#dbe6f3\", main = sprintf(%s, groupement))",
                code_texte(if (identical(o$dispersion, "mediane")) tx("distance à la médiane spatiale", "distance to the spatial median") else tx("distance au centroïde", "distance to the centroid")),
                code_texte(tx("Dispersion : %s", "Dispersion: %s"))),
        "}",
        "par(op)"))))))
  }
  if (m$type == "MANTEL") {
    nom_x <- if (r$geographique) tx("distance géographique (km)", "geographic distance (km)") else tx("distance X", "X distance")
    nom_y <- sprintf(tx("distance Y (%s)", "Y distance (%s)"), nom_distance_titre(o, langue))
    return(c(list(list(nom = "mantel", largeur = 6, hauteur = 5, code = t(c(
        sprintf("plot(Dx[lower.tri(Dx)], D[lower.tri(D)], pch = 20, cex = 0.6, col = \"#2f6fb540\", xlab = %s, ylab = %s,", code_texte(nom_x), code_texte(nom_y)),
        sprintf("     main = sprintf(%s, mantel$r, format(mantel$p, digits = 3)))", code_texte(tx("Test de Mantel : r = %.3f, p = %s", "Mantel test: r = %.3f, p = %s"))))))),
      if (!is.null(m$correlogramme)) list(list(nom = "correlogramme", largeur = 6, hauteur = 4, code = t(c(
        "cg <- correlogramme$table",
        "plot(cg$centre, cg$r, type = \"b\", pch = ifelse(!is.na(cg$p_corrige) & cg$p_corrige <= 0.05, 19, 1), col = \"#2f6fb5\",",
        sprintf("     xlab = %s, ylab = %s, main = %s)", code_texte(paste(nom_x, tx("(centre de la classe)", "(centre of the class)"))), code_texte(tx("r de Mantel", "Mantel r")),
                code_texte(tx("Corrélogramme de Mantel (plein : p corrigée ≤ 0,05)", "Mantel correlogram (filled: corrected p ≤ 0.05)"))),
        "abline(h = 0, col = \"grey\")"))))))
  }
  # les espèces indicatrices : la valeur de chaque espèce pour son groupe, de la couleur du groupe
  # (grise quand p > 0,05)
  list(list(nom = "valeurs-indicatrices", largeur = 8, hauteur = 5, code = t(c(
    "couleurs <- c(\"#2f6fb5\", \"#d1495b\", \"#3b9a5b\", \"#e09f3e\", \"#7b5ea7\", \"#2a9d8f\", \"#8c564b\", \"#c2549d\", \"#6b6b6b\", \"#a3a322\")",
    "ordre_especes <- order(-especes$indval)",
    "couleur <- couleurs[(match(especes$groupe, colnames(C)) - 1) %% 10 + 1]",
    if (o$permutations > 0) "couleur[is.na(especes$p) | especes$p > 0.05] <- \"grey85\"",
    "barplot(especes$indval[ordre_especes], names.arg = especes$espece[ordre_especes], las = 2, border = NA, ylim = c(0, 1), col = couleur[ordre_especes],",
    sprintf("        ylab = %s, main = %s)", code_texte(tx("valeur indicatrice", "indicator value")),
            code_texte(if (o$permutations > 0) tx("Valeurs indicatrices (en gris : p > 0,05)", "Indicator values (in grey: p > 0.05)") else tx("Valeurs indicatrices", "Indicator values"))),
    "vus <- unique(especes$groupe[ordre_especes])",
    "legend(\"topright\", legend = vus, fill = couleurs[(match(vus, colnames(C)) - 1) %% 10 + 1], border = NA, bty = \"n\", cex = 0.8)"))))
}

# ---------------------------------------------------------------- l'écart avec Nuée

# Les résultats du script (dans `e`) comparés à ceux de `m` : NULL, ou ce qui diffère.
ecart_communaute <- function(m, e, langue) {
  n <- function(x) nom_communautes(x, langue)
  r <- function(x) get0(n(x), envir = e, inherits = FALSE)
  champ <- function(x, ...) { for (k in c(...)) x <- if (is.list(x)) x[[n(k)]]; x }
  ajustement <- function(a) {
    if (is.null(a)) return(list())
    ra <- r("ajustement")
    rf <- champ(ra, "facteurs")
    list("variables ajustées" = if (!is.null(a$vecteurs)) list(cbind(a$vecteurs$direction, a$vecteurs$r2, a$vecteurs$p),
                                                               cbind(champ(ra, "vecteurs", "direction"), champ(ra, "vecteurs", "r2"), champ(ra, "vecteurs", "p"))),
         "facteurs ajustés" = if (!is.null(a$facteurs)) list(rbind(vapply(a$facteurs, `[[`, 0, "r2"), vapply(a$facteurs, `[[`, 0, "p")),
                                                             if (is.list(rf)) rbind(vapply(rf, function(f) as.numeric(f[[n("r2")]]), 0), vapply(rf, function(f) as.numeric(f[[n("p")]]), 0))),
         centroides = if (!is.null(a$centroides)) list(a$centroides, r("centroides")))
  }
  k <- m$noyau
  o <- if (identical(m$type, "INDVAL")) m$reglages_indval else m$options
  constante <- if (m$type %in% c("PCOA", "PERMANOVA") && !identical(o$correction, "aucune")) list(m$constante, champ(r("corr"), "constante"))
  paires <- switch(m$type,
    PCOA = c(list("valeurs propres" = list(k$valeurs, champ(r("pcoa"), "valeurs")), axes = list(k$points, champ(r("pcoa"), "points")),
                  especes = list(m$especes, r("especes")), constante = constante), ajustement(m$ajustement)),
    NMDS = c(list(stress = list(c(k$stress, k$meilleur, k$retrouvee, k$nuls), unlist(lapply(c("stress", "meilleur", "retrouvee", "nuls"), function(x) champ(r("nmds"), x)))),
                  departs = list(k$stress_departs, champ(r("nmds"), "stress_departs")), configuration = list(k$points, champ(r("nmds"), "points")),
                  "stress par observation" = list(k$stress_observations, champ(r("nmds"), "stress_observations")),
                  shepard = list(k$shepard$ajustee, champ(r("nmds"), "shepard", "ajustee")), especes = list(m$especes, r("especes"))), ajustement(m$ajustement)),
    PERMANOVA = {
      colonnes <- c("ddl", "ss", "R2", "F", "p")
      ts <- champ(r("permanova"), "table")
      ds <- r("dispersions")
      c(list(permanova = list(as.matrix(k$table[colonnes]), if (is.data.frame(ts)) as.matrix(ts[n(colonnes)])),
             modele = list(unlist(k$modele), unlist(champ(r("permanova"), "modele"))), constante = constante),
        stats::setNames(lapply(names(m$dispersions), function(nom) {
          x <- m$dispersions[[nom]]
          y <- if (is.list(ds)) ds[[nom]]
          tk <- c("difference", "bas", "haut", "p")
          list(c(x$distances, x$F, x$p, x$p_permutation, as.matrix(x$tukey[tk])),
               if (is.list(y)) c(champ(y, "distances"), y$F, champ(y, "p"), champ(y, "p_permutation"), as.matrix(champ(y, "tukey")[n(tk)])))
        }), sprintf("dispersion %s", names(m$dispersions))))
    },
    MANTEL = {
      colonnes <- c("centre", "paires", "r", "p", "p_corrige")
      cs <- champ(r("correlogramme"), "table")
      list(mantel = list(unlist(m$test[c("r", "p", "r_partiel", "p_partiel")]), unlist(lapply(c("r", "p", "r_partiel", "p_partiel"), function(x) champ(r("mantel"), x)))),
           correlogramme = if (!is.null(m$correlogramme)) list(as.matrix(m$correlogramme$table[colonnes]), if (is.data.frame(cs)) as.matrix(cs[n(colonnes)])))
    },
    INDVAL = list(A = list(m$A, champ(r("statistiques"), "A")), B = list(m$B, champ(r("statistiques"), "B")), IndVal = list(m$IndVal, champ(r("statistiques"), "IndVal")),
                  meilleures = list(cbind(m$meilleure, m$valeur), cbind(champ(r("meilleures"), "rang"), champ(r("meilleures"), "valeur"))),
                  p = list(cbind(m$p, m$p_ajuste), cbind(r("p"), r("p_ajuste")))))
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

# ---------------------------------------------------------------- écrire le script

# (les cinq analyses s'exportent par la même fonction)
ecrire_script_pcoa <- ecrire_script_nmds <- ecrire_script_permanova <- ecrire_script_mantel <- ecrire_script_indval <- function(...) ecrire_script_communaute(...)

ecrire_script_communaute <- function(projet, d, chemin_script, m, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (langue == "en") en else fr
  format <- c(r = "R", rmd = "Rmd", qmd = "qmd")[[tolower(tools::file_ext(chemin_script))]]
  r <- reglages_script(m)
  st <- r$o$strates
  # (avec l'identifiant des groupes des permutations, ceux que lit sa condition, et les fonctions de
  # Nouvelle variable qu'elle appelle : condition.R)
  tab <- tableau_script(projet, unique(c(if (!is.null(st) && identical(as.character(st$by)[1], "id")) as.character(st$id)[1], ids_condition(projet, d))))
  fichiers <- fichiers_script(chemin_script, donnees, rdata, renv, rproj, langue)
  code_donnees <- donnees_script(tab, fichiers, langue)
  fc <- plan_condition(projet, d, tab)$fonctions
  fonctions <- traduire_communautes(c(code_fonctions_communaute(m, langue), if (length(fc)) code_fonctions_condition(fc, tx)), langue)
  calcul <- traduire_communautes(code_calcul_communaute(projet, d, m, tab, langue), langue)
  e <- environnement_script(tab, fonctions, langue)
  # (le script tire ses permutations et ses départs de sa graine ; le hasard de la séance est rendu après)
  probleme <- avec_graine(1, evaluer_script(calcul, e, avertissements = FALSE))
  ecart <- if (!is.null(probleme)) probleme else ecart_communaute(m, e, langue)
  sortie <- code_sortie_communaute(m, langue)
  graphiques <- code_graphiques_communaute(m, langue)
  nom <- texte_sur(as.character(d$name %||% ""))
  titre <- texte_sur(as.character(d$title %||% ""))
  type <- switch(m$type,
    PCOA = tx("analyse en coordonnées principales (PCoA)", "principal coordinates analysis (PCoA)"),
    NMDS = tx("positionnement multidimensionnel non métrique (NMDS)", "non-metric multidimensional scaling (NMDS)"),
    PERMANOVA = tx("analyse de variance multivariée par permutation (PERMANOVA)", "permutational multivariate analysis of variance (PERMANOVA)"),
    MANTEL = tx("test de Mantel", "Mantel test"), INDVAL = tx("espèces indicatrices (IndVal)", "indicator species (IndVal)"))
  nmds <- identical(m$type, "NMDS")
  avec <- if (nmds) tx("R et son paquet MASS (livré avec R)", "R and its package MASS (it comes with R)") else tx("R seul", "R alone")
  paquet <- if (identical(m$type, "INDVAL")) "indicspecies (multipatt)" else "vegan"
  entete <- c(sprintf(tx("# Modèle %s%s : %s, %d observations", "# Model %s%s: %s, %d observations"), nom, if (nzchar(titre)) sprintf(" (%s)", titre) else "", type, m$N),
              sprintf(tx("# Script R écrit par Nuée %s le %s, à partir du projet « %s ».", "# R script written by Nuée %s on %s, from the project “%s”."), version, format(Sys.Date()),
                      texte_sur(as.character(projet$source %||% "projet"))),
              commentaire(tx(sprintf("Il refait cette analyse avec %s, sans Nuée : le calcul est celui du paquet %s, écrit pour ce modèle, de ses données à ses résultats, qui sont ceux de Nuée (pour les mêmes permutations, les mêmes valeurs p). Ses quatre parties :", avec, paquet),
                             sprintf("It redoes this analysis with %s, without Nuée: the computation is that of the %s package, written for this model, from its data to its results, which are those of Nuée (for the same permutations, the same p-values). Its four parts:", avec, paquet))),
              tx("#   1. Fonctions : celles du moteur de Nuée (moteur/R), telles qu'il les écrit", "#   1. Functions: those of Nuée's engine (moteur/R), as it writes them"),
              tx("#   2. Données", "#   2. Data"),
              tx("#   3. Modèle : variables, observations, calcul", "#   3. Model: variables, observations, computation"),
              tx("#   4. Résultats : tableaux et graphiques", "#   4. Results: tables and plots"),
              tx("# Dans RStudio, Ctrl+Maj+O affiche ces parties ; Ctrl+Maj+S lance tout le script.", "# In RStudio, Ctrl+Shift+O shows these parts; Ctrl+Shift+S runs the whole script."),
              sprintf(tx("# Écrit avec R %s%s.", "# Written with R %s%s."), paste(R.version$major, R.version$minor, sep = "."),
                      if (nmds) sprintf(tx(" et le paquet MASS %s, livré avec R (appelé par MASS::)", " and the package MASS %s, which comes with R (called as MASS::)"), utils::packageVersion("MASS"))
                      else tx(" ; il n'utilise que R (stats et graphics sont livrés avec R)", "; it uses R alone (stats and graphics come with R)")),
              fichiers_a_cote(fichiers, langue, FALSE))
  lignes <- if (format == "R") {
    c(entete, "", titre_partie(1, langue), fonctions, titre_partie(2, langue), code_donnees, "", titre_partie(3, langue), calcul, "",
      titre_partie(4, langue), sortie, "", texte_graphiques(langue), unlist(lapply(graphiques, function(g) c(g$code, ""))))
  } else {
    lignes_rapport_communaute(format, d, m, langue, texte_methodes(projet, d, m, langue, version), fonctions, code_donnees, calcul, graphiques)
  }
  objets <- nom_communautes(c("Y", "W", "D", "corr", "pcoa", "nmds", "valeurs_propres", "especes", "Xq", "facteurs", "ajustement", "centroides", "variables_ajustees",
                              "facteurs_ajustes", "X", "termes_x", "Z", "coordonnees", "permutations", "G", "permanova", "dispersions", "Dx", "Dz", "mantel",
                              "correlogramme", "groupe", "C", "statistiques", "meilleures", "p", "p_ajuste", "absentes"), langue)
  ecrire_fichiers_script(lignes, fichiers, e, c(nom_script("donnees", langue), objets), c(if (nmds) "MASS", if (format != "R") c("knitr", "rmarkdown")))
  # (ses graphiques, pour un document reproductible : document_reproductible.R)
  structure(list(script = chemin_script, chemins = unname(fichiers), ecart = ecart), graphiques = graphiques)
}

# Le rapport R Markdown ou Quarto : les méthodes, les données, le code (replié dans le rapport
# compilé), les résultats (tableaux, graphiques en R de base) et les références.
lignes_rapport_communaute <- function(format, d, m, langue, methodes, fonctions, code_donnees, calcul, graphiques) {
  tx <- function(fr, en) if (langue == "en") en else fr
  bloc <- function(options, code) c(sprintf("```{r%s}", if (nzchar(options)) paste0(" ", options) else ""), code, "```", "")
  kable <- function(objet, chiffres) traduire_communautes(sprintf("knitr::kable(%s, digits = %d)", objet, chiffres), langue)
  nom <- texte_sur(as.character(d$name %||% ""))
  titre <- sprintf("%s %s%s", tx("Modèle", "Model"), nom, if (nzchar(as.character(d$title %||% ""))) paste(" —", texte_sur(d$title)) else "")
  yaml <- c("---", sprintf("title: %s", code_texte(titre)), sprintf("date: %s", code_texte(format(Sys.Date()))), sprintf("lang: %s", langue),
            if (format == "Rmd") c("output:", "  html_document:", "    code_folding: hide") else c("format:", "  html:", "    embed-resources: true", "    code-fold: true"),
            "---", "")
  a <- m$ajustement
  variables <- c(if (!is.null(a$vecteurs)) bloc("variables-table", kable("variables_ajustees", 3)), if (!is.null(a$facteurs)) bloc("facteurs-table", kable("facteurs_ajustes", 3)))
  resultats <- switch(m$type,
    PCOA = c(bloc("valeurs-propres-table", kable("head(valeurs_propres, 10)", 4)), variables),
    NMDS = c(bloc("stress-table", kable("data.frame(stress = nmds$stress, meilleur = nmds$meilleur, retrouvee = nmds$retrouvee)", 4)), variables),
    PERMANOVA = c(bloc("permanova-table", kable("permanova$table", 3)),
                  unlist(lapply(seq_along(m$dispersions), function(k) {
                    q <- sprintf("dispersions[[%s]]", code_texte(names(m$dispersions)[k]))
                    c(bloc(sprintf("dispersion-%d-anova", k), kable(sprintf("%s$anova", q), 3)), bloc(sprintf("dispersion-%d-paires", k), kable(sprintf("%s$tukey", q), 3)))
                  }))),
    MANTEL = c(bloc("mantel-table", kable("t(unlist(mantel))", 4)), if (!is.null(m$correlogramme)) bloc("correlogramme-table", kable("correlogramme$table", 3))),
    INDVAL = bloc("especes-table", kable("especes", 3)))
  c(yaml,
    sprintf("## %s", tx("Méthodes", "Methods")), "", rbind(texte_sur(methodes$paragraphes), ""),
    sprintf("## %s", tx("Données", "Data")), "",
    bloc("donnees, echo = FALSE", code_donnees),
    bloc("", traduire_code("str(donnees, list.len = 12)", langue)),
    sprintf("## %s", tx("Modèle", "Model")), "",
    tx("Le calcul, écrit en R pour ce modèle (bouton « Code » pour le voir).", "The computation, written in R for this model (“Code” button to see it)."), "",
    bloc("fonctions", fonctions[seq_len(max(0, length(fonctions) - 1))]),
    bloc("modele", calcul),
    sprintf("## %s", tx("Résultats", "Results")), "",
    resultats,
    unlist(lapply(graphiques, function(g) bloc(sprintf("%s, fig.width = %s, fig.height = %s", g$nom, g$largeur, g$hauteur), g$code))),
    sprintf("## %s", tx("Références", "References")), "", paste("-", methodes$references))
}
