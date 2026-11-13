# Séries dans le temps (Graphiques › Séries) : les valeurs d'une variable au fil du temps, une
# ligne par groupe (le secteur) et un petit graphique par groupe plus large (le site), comme des
# « séries par secteur, une page par groupe ». Le moteur rend les données en long, une ligne par
# observation dont le temps et la valeur sont connus ; l'interface trace les lignes, et coupe une
# ligne là où un temps manque. Au choix, la tendance de chaque ligne : le test de Mann–Kendall et
# la pente de Sen (tendances_series).
#   variable : le numéro d'une variable quantitative ;
#   temps : le numéro d'une variable, ou "id:k" pour le k-ième identifiant des observations
#     (l'année), fait de nombres ou de dates (temps_en_nombres : en années décimales ; `dates` le
#     dit, et l'interface les montre en dates) ;
#   lignes, panneaux : le numéro d'une variable qualitative ou "id:k" (le secteur, le site), ou
#     NULL : une seule ligne, un seul panneau.
# Renvoie les points (temps, valeur, ligne, panneau, uid et nom de l'observation), rangés par
# panneau, ligne et temps (l'interface coupe une ligne là où deux de ses temps sont plus éloignés
# que son pas, le plus petit écart entre ses temps ; des dates, rarement régulières, ne la coupent
# pas) ; les niveaux des lignes et des panneaux, dans leur ordre ; la moyenne des lignes de chaque
# panneau à chaque temps ; le nombre d'observations laissées de côté (sans temps ou sans valeur ;
# sans ligne ou sans panneau) ; les temps qui reviennent dans une même ligne (doubles : leur nombre
# et le premier).
series_variable <- function(projet, variable, temps, lignes = NULL, panneaux = NULL) {
  obs <- numeros_observations(projet)
  v <- valeurs_a_tester(projet, variable, obs)
  if (v$qualitative) stop(traduire("Choisir une variable quantitative", "Choose a quantitative variable"), call. = FALSE)
  tp <- valeurs_a_tester(projet, temps, obs)
  t <- temps_en_nombres(tp$valeurs)
  if (!is.null(attr(t, "refus"))) stop(refus_temps(t, tp$nom), call. = FALSE)
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
  list(variable = v[c("uid", "nom")], temps = tp[c("uid", "nom")], dates = isTRUE(attr(t, "dates")),
       lignes = if (!is.null(l)) l[c("uid", "nom")], panneaux = if (!is.null(pa)) pa[c("uid", "nom")],
       points = points, niveaux_lignes = I(niveaux(l)), niveaux_panneaux = I(niveaux(pa)),
       moyennes = moyennes,
       sans_temps_valeur = sum(!connus), sans_groupe = sum(connus & !avec_groupe), doubles = doubles)
}

# Les tendances des lignes (Graphiques › Séries, « Tendances ») : `points`, ceux de series_variable(),
# rangés par panneau, ligne et temps. Pour chaque ligne de chaque panneau, dans cet ordre : n (ses
# temps qui ont une valeur), son premier et son dernier temps ;
#   - la pente de Sen (Sen, 1968) : la médiane des pentes (y_j − y_i) / (t_j − t_i) de toutes ses
#     paires de temps, par unité de temps (par an avec des dates), et son intervalle à 95 %
#     (Gilbert, 1987 ; Hollander et Wolfe, 1999) : les N pentes rangées, la M-ième et la
#     (N + 1 − M)-ième, M = (N − C) / 2 arrondi, C = 1,96 √Var(S) ; sans bornes quand M < 1 (trop
#     peu de temps pour un intervalle à 95 %) ;
#   - le test de Mann–Kendall : S, la somme des signes de y_j − y_i sur les paires de temps i avant
#     j ; sa variance corrigée pour les ex aequo de y, Var(S) = [n(n − 1)(2n + 5) − Σ t(t − 1)(2t + 5)]
#     / 18 (t : le nombre de valeurs de chaque groupe d'ex aequo) ; sa valeur p bilatérale par la loi
#     normale avec la correction de continuité, z = (S − signe(S)) / √Var(S), comme trend::mk.test
#     et Kendall::MannKendall (S et Var(S) calculés ici : la p exacte de cor.test(method =
#     "kendall") n'est pas la même sous toutes les versions de R) ; le τ de Kendall (tau-b, comme
#     Kendall::MannKendall et cor(method = "kendall") : S sur la racine du produit des paires sans ex
#     aequo, n(n − 1)/2 en temps et n(n − 1)/2 − Σ t(t − 1)/2 en valeurs) ;
#   - la valeur p corrigée sur les lignes testées : correction = "BH" (Benjamini et Hochberg, 1995)
#     ou "holm" (Holm, 1979).
# Une ligne de moins de 4 temps, qui a un même temps deux fois (ce n'est pas une série), ou qui a la
# même valeur à chaque temps n'est pas testée : non_teste dit pourquoi. Le test tient les temps
# pour indépendants : une autocorrélation positive (des années qui se ressemblent) rend p trop
# petite ; un modèle mixte avec une autocorrélation CAR1 en tient compte.
tendances_series <- function(points, correction = "BH") {
  if (!correction %in% c("BH", "holm")) stop(traduire("Correction inconnue : ", "Unknown correction: "), correction, call. = FALSE)
  if (!nrow(points)) return(data.frame())
  cle <- paste(points$panneau, points$ligne, sep = "\r")
  res <- do.call(rbind, lapply(split(seq_len(nrow(points)), factor(cle, levels = unique(cle))), function(i) {
    tt <- points$temps[i]
    y <- points$valeur[i]
    n <- length(i)
    liens <- rle(sort(y))$lengths  # (les ex aequo exacts de y)
    r <- data.frame(panneau = points$panneau[i[1]], ligne = points$ligne[i[1]], n = n, debut = min(tt), fin = max(tt),
                    pente = NA_real_, bas = NA_real_, haut = NA_real_, tau = NA_real_, S = NA_real_, var_S = NA_real_, p = NA_real_,
                    non_teste = if (n < 4) sprintf(traduire("%d temps : il en faut au moins 4", "%d times: at least 4 are needed"), n)
                                else if (anyDuplicated(round(tt, 8))) traduire("un même temps deux fois : pas une série", "the same time twice: not a series")
                                else if (length(liens) == 1) traduire("la même valeur à chaque temps", "the same value at every time")
                                else "", stringsAsFactors = FALSE)
    if (nzchar(r$non_teste)) return(r)
    # toutes les paires de temps, le plus tard moins le plus tôt (la ligne est rangée par temps)
    dy <- outer(y, y, "-")
    dt <- outer(tt, tt, "-")
    paires <- lower.tri(dy)
    pentes <- sort(dy[paires] / dt[paires])
    S <- sum(sign(dy[paires]))
    var_S <- (n * (n - 1) * (2 * n + 5) - sum(liens * (liens - 1) * (2 * liens + 5))) / 18
    N <- length(pentes)
    M <- round((N - stats::qnorm(0.975) * sqrt(var_S)) / 2)
    r[c("pente", "tau", "S", "var_S", "p")] <- list(stats::median(pentes), S / sqrt(N * (N - sum(liens * (liens - 1) / 2))), S, var_S,
                                                    2 * stats::pnorm(-abs((S - sign(S)) / sqrt(var_S))))
    if (M >= 1) r[c("bas", "haut")] <- list(pentes[M], pentes[N + 1 - M])
    r
  }))
  rownames(res) <- NULL
  res$p_ajuste <- stats::p.adjust(res$p, method = correction)
  res
}
