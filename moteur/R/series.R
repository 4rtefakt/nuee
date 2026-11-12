# Séries dans le temps (Graphiques › Séries) : les valeurs d'une variable au fil du temps, une
# ligne par groupe (le secteur) et un petit graphique par groupe plus large (le site), comme des
# « séries par secteur, une page par groupe ». Le moteur rend les données en long, une ligne par
# observation dont le temps et la valeur sont connus ; l'interface trace les lignes, et coupe une
# ligne là où un temps manque.
#   variable : le numéro d'une variable quantitative ;
#   temps : le numéro d'une variable quantitative, ou "id:k" pour le k-ième identifiant des
#     observations (l'année), fait de nombres ;
#   lignes, panneaux : le numéro d'une variable qualitative ou "id:k" (le secteur, le site), ou
#     NULL : une seule ligne, un seul panneau.
# Renvoie les points (temps, valeur, ligne, panneau, uid et nom de l'observation), rangés par
# panneau, ligne et temps ; les niveaux des lignes et des panneaux, dans leur ordre ; le pas des
# temps (le plus petit écart entre deux temps différents : dans une ligne, deux temps plus éloignés
# laissent un trou) ; la moyenne des lignes de chaque panneau à chaque temps ; le nombre
# d'observations laissées de côté (sans temps ou sans valeur ; sans ligne ou sans panneau) ; les
# temps qui reviennent dans une même ligne (doubles : leur nombre et le premier).
series_variable <- function(projet, variable, temps, lignes = NULL, panneaux = NULL) {
  obs <- numeros_observations(projet)
  v <- valeurs_a_tester(projet, variable, obs)
  if (v$qualitative) stop(traduire("Choisir une variable quantitative", "Choose a quantitative variable"), call. = FALSE)
  tp <- valeurs_a_tester(projet, temps, obs)
  t <- temps_en_nombres(if (is.factor(tp$valeurs)) as.character(tp$valeurs) else tp$valeurs)
  if (!is.null(attr(t, "refus"))) {
    stop(sprintf(traduire("Le temps doit être numérique : « %s » (%s) n'est pas un nombre", "The time must be numeric: “%s” (%s) isn't a number"),
                 attr(t, "refus"), tp$nom), call. = FALSE)
  }
  groupe <- function(g) {
    if (!length(g) || identical(as.character(g), "")) return(NULL)
    x <- valeurs_a_tester(projet, g, obs)
    if (!x$qualitative) stop(traduire("Les lignes et les panneaux se font par une variable qualitative ou un identifiant", "Lines and panels are made by a qualitative variable or an ID"), call. = FALSE)
    x
  }
  l <- groupe(lignes)
  pa <- groupe(panneaux)
  y <- v$valeurs
  connus <- !is.na(t) & !is.na(y)
  dans <- function(x) if (is.null(x)) rep(TRUE, length(obs)) else !is.na(x$valeurs)
  avec_groupe <- dans(l) & dans(pa)
  # (les numéros des niveaux, pour ranger les points par panneau, ligne et temps)
  cl <- if (is.null(l)) rep(1L, length(obs)) else as.integer(l$valeurs)
  cp <- if (is.null(pa)) rep(1L, length(obs)) else as.integer(pa$valeurs)
  i <- which(connus & avec_groupe)
  i <- i[order(cp[i], cl[i], t[i])]
  niveau <- function(x) if (is.null(x)) rep(NA_character_, length(i)) else as.character(x$valeurs[i])
  points <- data.frame(temps = t[i], valeur = y[i], ligne = niveau(l), panneau = niveau(pa), uid = obs[i],
                       nom = unname(projet$noms_obs[as.character(obs[i])]), stringsAsFactors = FALSE)
  niveaux <- function(x) if (is.null(x)) character(0) else levels(droplevels(x$valeurs[i]))
  temps_cle <- as.character(round(t[i], 8))
  u <- sort(unique(round(t[i], 8)))
  # un même temps deux fois dans une ligne : ce n'est pas une série (les lignes à choisir plus fines)
  ligne_cle <- paste(cp[i], cl[i], temps_cle, sep = "\r")
  d <- which(duplicated(ligne_cle))
  doubles <- list(n = length(d))
  if (length(d)) doubles <- c(doubles, list(ligne = points$ligne[d[1]], panneau = points$panneau[d[1]], temps = points$temps[d[1]]))
  # la moyenne des lignes de chaque panneau, temps par temps (les observations présentes ce temps-là)
  k <- paste(cp[i], temps_cle, sep = "\r")
  premiers <- which(!duplicated(k))
  premiers <- premiers[order(cp[i][premiers], t[i][premiers])]
  moyennes <- data.frame(panneau = points$panneau[premiers], temps = points$temps[premiers],
                         valeur = as.numeric(tapply(points$valeur, k, mean)[k[premiers]]),
                         n = as.integer(tapply(points$valeur, k, length)[k[premiers]]), stringsAsFactors = FALSE)
  list(variable = v[c("uid", "nom")], temps = tp[c("uid", "nom")],
       lignes = if (!is.null(l)) l[c("uid", "nom")], panneaux = if (!is.null(pa)) pa[c("uid", "nom")],
       points = points, niveaux_lignes = I(niveaux(l)), niveaux_panneaux = I(niveaux(pa)),
       pas = if (length(u) > 1) min(diff(u)) else NA_real_, moyennes = moyennes,
       sans_temps_valeur = sum(!connus), sans_groupe = sum(connus & !avec_groupe), doubles = doubles)
}
