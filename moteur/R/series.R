# Séries dans le temps (Graphiques › Séries) : les valeurs d'une variable au fil du temps, une
# ligne par groupe (le secteur) et un petit graphique par groupe plus large (le site), comme des
# « séries par secteur, une page par groupe ». Le moteur rend les données en long, une ligne par
# observation dont le temps et la valeur sont connus ; l'interface trace les lignes, et coupe une
# ligne là où un temps manque. Au choix, la tendance de chaque ligne : le test de Mann–Kendall et
# la pente de Sen, avec au choix la correction de Hamed et Rao pour l'autocorrélation, ou le test de
# Kendall saisonnier (tendances_series).
#   variable : le numéro d'une variable quantitative ;
#   temps : le numéro d'une variable, ou "id:k" pour le k-ième identifiant des observations
#     (l'année), fait de nombres ou de dates (temps_en_nombres : en années décimales ; `dates` le
#     dit, et l'interface les montre en dates) ;
#   lignes, panneaux : le numéro d'une variable qualitative ou "id:k" (le secteur, le site), ou
#     NULL : une seule ligne, un seul panneau ;
#   saisons : pour le test saisonnier, "mois" (le mois de l'année d'un temps fait de dates), le
#     numéro d'une variable qualitative ou "id:k" (la saison, le mois) : la saison de chaque point
#     (ajouter_saisons) ; NULL : aucune.
# Renvoie les points (temps, valeur, ligne, panneau, uid et nom de l'observation, et leur saison),
# rangés par panneau, ligne et temps (l'interface coupe une ligne là où deux de ses temps sont plus
# éloignés que son pas, le plus petit écart entre ses temps ; des dates, rarement régulières, ne la
# coupent pas) ; les niveaux des lignes et des panneaux, dans leur ordre ; la moyenne des lignes de
# chaque panneau à chaque temps ; le nombre d'observations laissées de côté (sans temps ou sans
# valeur ; sans ligne ou sans panneau) ; les temps qui reviennent dans une même ligne (doubles : leur
# nombre et le premier).
series_variable <- function(projet, variable, temps, lignes = NULL, panneaux = NULL, saisons = NULL) {
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
  r <- list(variable = v[c("uid", "nom")], temps = tp[c("uid", "nom")], dates = isTRUE(attr(t, "dates")),
            lignes = if (!is.null(l)) l[c("uid", "nom")], panneaux = if (!is.null(pa)) pa[c("uid", "nom")],
            points = points, niveaux_lignes = I(niveaux(l)), niveaux_panneaux = I(niveaux(pa)),
            moyennes = moyennes,
            sans_temps_valeur = sum(!connus), sans_groupe = sum(connus & !avec_groupe), doubles = doubles)
  if (length(saisons) && nzchar(as.character(saisons))) r <- ajouter_saisons(projet, r, saisons)
  r
}

# La saison de chaque point d'une série (`serie` : le résultat de series_variable), pour le test de
# Kendall saisonnier : avec saisons = "mois", le mois de l'année de sa date (1 à 12 ; le temps doit
# être fait de dates) ; sinon la catégorie de la variable qualitative ou de l'identifiant `saisons`
# (la saison, le mois), manquante pour une observation qui n'en a pas. Renvoie la série, ses points
# avec une colonne saison, ce qui fait les saisons (saisons : uid et nom) et le nombre de points
# sans saison (sans_saison), que le test laisse de côté.
ajouter_saisons <- function(projet, serie, saisons) {
  if (identical(as.character(saisons), "mois")) {
    if (!isTRUE(serie$dates)) stop(traduire("Les saisons par mois de l'année demandent un temps fait de dates",
                                            "Seasons by month of the year need a time made of dates"), call. = FALSE)
    s <- as.integer(substr(date_decimale(serie$points$temps), 6, 7))
    serie$saisons <- list(uid = "mois", nom = traduire("mois", "month"))
  } else {
    x <- valeurs_a_tester(projet, saisons, serie$points$uid)
    if (!x$qualitative) stop(traduire("Les saisons se font par une variable qualitative ou un identifiant", "Seasons are made by a qualitative variable or an ID"), call. = FALSE)
    s <- as.character(x$valeurs)
    serie$saisons <- x[c("uid", "nom")]
  }
  serie$points$saison <- s
  serie$sans_saison <- sum(is.na(s))
  serie
}

# Les tendances des lignes (Graphiques › Séries, « Tendances ») : `points`, ceux de series_variable(),
# rangés par panneau, ligne et temps. Pour chaque ligne de chaque panneau, dans cet ordre : n (ses
# temps qui ont une valeur), son premier et son dernier temps, le test qu'elle a eu (test), et :
#   - sans saisons, le test de Mann–Kendall et la pente de Sen (mann_kendall : "mann_kendall") ;
#     avec autocorrelation = TRUE, le test modifié de Hamed et Rao (1998), sa correction n/n* de la
#     variance de S (n_n_etoile ; "hamed_rao") ;
#   - quand les points ont une saison (series_variable(saisons = ...)), le test de Kendall
#     saisonnier (kendall_saisonnier : "saisonnier" ; saisons, le nombre de saisons qui comptent ;
#     medianes, les couples saison–année de plusieurs valeurs), les points sans saison laissés de
#     côté ;
#   - la valeur p corrigée sur les lignes testées : correction = "BH" (Benjamini et Hochberg, 1995)
#     ou "holm" (Holm, 1979).
# Une ligne qui n'est pas testée le dit (non_teste). Le test saisonnier tient les saisons pour
# indépendantes : la correction de Hamed et Rao ne s'y ajoute pas (refusée).
tendances_series <- function(points, correction = "BH", autocorrelation = FALSE) {
  if (!correction %in% c("BH", "holm")) stop(traduire("Correction inconnue : ", "Unknown correction: "), correction, call. = FALSE)
  saisonnier <- "saison" %in% names(points)
  if (saisonnier && isTRUE(autocorrelation)) {
    stop(traduire("Le test saisonnier tient les saisons pour indépendantes : la correction de Hamed et Rao pour l'autocorrélation ne s'y ajoute pas",
                  "The seasonal test takes the seasons as independent: Hamed and Rao's correction for autocorrelation doesn't apply to it"), call. = FALSE)
  }
  if (saisonnier) points <- points[!is.na(points$saison), , drop = FALSE]
  if (!nrow(points)) return(data.frame())
  test <- if (saisonnier) "saisonnier" else if (isTRUE(autocorrelation)) "hamed_rao" else "mann_kendall"
  cle <- paste(points$panneau, points$ligne, sep = "\r")
  res <- do.call(rbind, lapply(split(seq_len(nrow(points)), factor(cle, levels = unique(cle))), function(i) {
    tt <- points$temps[i]
    k <- if (saisonnier) kendall_saisonnier(tt, points$valeur[i], points$saison[i]) else mann_kendall(tt, points$valeur[i], isTRUE(autocorrelation))
    data.frame(panneau = points$panneau[i[1]], ligne = points$ligne[i[1]], n = length(i), debut = min(tt), fin = max(tt), test = test,
               k, stringsAsFactors = FALSE)
  }))
  rownames(res) <- NULL
  res$p_ajuste <- stats::p.adjust(res$p, method = correction)
  res
}

# Le test de Mann–Kendall d'une ligne (ses temps t, rangés, et ses valeurs y) et sa pente de Sen :
#   - la pente de Sen (Sen, 1968) : la médiane des pentes (y_j − y_i) / (t_j − t_i) de toutes ses
#     paires de temps, par unité de temps (par an avec des dates), et son intervalle à 95 %
#     (intervalle_sen) ;
#   - S, la somme des signes de y_j − y_i sur les paires de temps i avant j ; sa variance corrigée
#     pour les ex aequo de y, Var(S) = [n(n − 1)(2n + 5) − Σ t(t − 1)(2t + 5)] / 18 (t : le nombre de
#     valeurs de chaque groupe d'ex aequo) ; sa valeur p bilatérale par la loi normale avec la
#     correction de continuité, z = (S − signe(S)) / √Var(S), comme trend::mk.test et
#     Kendall::MannKendall (S et Var(S) calculés ici : la p exacte de cor.test(method = "kendall")
#     n'est pas la même sous toutes les versions de R) ; le τ de Kendall (tau-b, comme
#     Kendall::MannKendall et cor(method = "kendall") : S sur la racine du produit des paires sans
#     ex aequo, n(n − 1)/2 en temps et n(n − 1)/2 − Σ t(t − 1)/2 en valeurs) ;
#   - avec autocorrelation, le test modifié de Hamed et Rao (1998) : Var(S) multipliée par n/n*
#     (facteur_hamed_rao), la valeur p et l'intervalle de la pente tirés de cette variance.
# Une ligne de moins de 4 temps, qui a un même temps deux fois (ce n'est pas une série), ou qui a
# la même valeur à chaque temps n'est pas testée : non_teste dit pourquoi ; de même quand n/n* n'est
# pas positif (une forte autocorrélation négative, hors de ce que la formule corrige). Sans la
# correction, le test tient les temps pour indépendants : une autocorrélation positive (des années
# qui se ressemblent) rend p trop petite.
mann_kendall <- function(t, y, autocorrelation = FALSE) {
  n <- length(y)
  liens <- rle(sort(y))$lengths  # (les ex aequo exacts de y)
  r <- list(pente = NA_real_, bas = NA_real_, haut = NA_real_, tau = NA_real_, S = NA_real_, var_S = NA_real_, p = NA_real_,
            n_n_etoile = NA_real_, saisons = NA_integer_, medianes = NA_integer_,
            non_teste = if (n < 4) sprintf(traduire("%d temps : il en faut au moins 4", "%d times: at least 4 are needed"), n)
                        else if (anyDuplicated(round(t, 8))) traduire("un même temps deux fois : pas une série", "the same time twice: not a series")
                        else if (length(liens) == 1) traduire("la même valeur à chaque temps", "the same value at every time")
                        else "")
  if (nzchar(r$non_teste)) return(r)
  # toutes les paires de temps, le plus tard moins le plus tôt (la ligne est rangée par temps)
  dy <- outer(y, y, "-")
  dt <- outer(t, t, "-")
  paires <- lower.tri(dy)
  pentes <- sort(dy[paires] / dt[paires])
  S <- sum(sign(dy[paires]))
  var_S <- (n * (n - 1) * (2 * n + 5) - sum(liens * (liens - 1) * (2 * liens + 5))) / 18
  N <- length(pentes)
  r[c("pente", "tau", "S")] <- list(stats::median(pentes), S / sqrt(N * (N - sum(liens * (liens - 1) / 2))), S)
  if (autocorrelation) {
    r$n_n_etoile <- facteur_hamed_rao(t, y, r$pente)
    if (!(r$n_n_etoile > 0)) {
      r$non_teste <- sprintf(traduire("n/n* = %s n'est pas positif (une forte autocorrélation négative) : la correction de Hamed et Rao ne s'applique pas",
                                      "n/n* = %s isn't positive (a strong negative autocorrelation): Hamed and Rao's correction doesn't apply"),
                             formatC(r$n_n_etoile, digits = 3, format = "g", decimal.mark = traduire(",", ".")))
      return(r)
    }
    var_S <- var_S * r$n_n_etoile
  }
  r$var_S <- var_S
  r$p <- 2 * stats::pnorm(-abs((S - sign(S)) / sqrt(var_S)))
  r[c("bas", "haut")] <- as.list(intervalle_sen(pentes, var_S))
  r
}

# Le facteur n/n* de Hamed et Rao (1998), comme le calcule modifiedmk::mmkh : la ligne (temps t
# rangés, valeurs y) privée de sa tendance, y − pente × (t − t_1), `pente` étant sa pente de Sen ;
# les rangs de ce qui reste ; leur autocorrélation ρ(k) à chaque décalage k de 1 à n − 1
# (stats::acf, les temps pris dans leur ordre : un décalage va d'un temps au suivant, comme d'une
# valeur à la suivante dans le vecteur de mmkh) ; celles qui ne dépassent pas ±qnorm(0,975)/√n
# comptent 0 ; puis n/n* = 1 + 2 / (n(n − 1)(n − 2)) Σ (n − k)(n − k − 1)(n − k − 2) ρ(k). Sur des
# temps réguliers, ce qui reste est la série de mmkh (qui compte le temps 1, 2, 3...) à une
# constante près : les mêmes rangs. Ce qui reste est arrondi à 12 chiffres avant d'être rangé
# (deux valeurs égales le restent malgré les erreurs d'arrondi du calcul) ; ce qui reste d'une
# ligne droite est constant : aucune autocorrélation, n/n* = 1.
facteur_hamed_rao <- function(t, y, pente) {
  n <- length(y)
  rangs <- rank(signif(y - pente * (t - t[1]), 12))
  rho <- stats::acf(rangs, lag.max = n - 1, plot = FALSE)$acf[-1]
  rho[is.na(rho) | abs(rho) <= stats::qnorm(0.975) / sqrt(n)] <- 0
  k <- seq_len(n - 1)
  1 + 2 / (n * (n - 1) * (n - 2)) * sum((n - k) * (n - k - 1) * (n - k - 2) * rho)
}

# Le test de Kendall saisonnier d'une ligne (Hirsch, Slack et Smith, 1982) : ses temps t, ses
# valeurs y et la saison de chacune. L'année d'une valeur est la partie entière de son temps
# (l'année civile d'une date). Plusieurs valeurs d'une même saison la même année n'en font qu'une,
# leur médiane (medianes : combien de couples saison–année en avaient plusieurs). Dans chaque saison
# vue au moins deux années (les autres ne comptent pas) : S_g, la somme des signes entre ses années,
# sa variance avec les ex aequo (comme mann_kendall), les pentes entre ses années (par an, ou par
# unité de temps). Puis S = Σ S_g et Var(S) = Σ Var(S_g) (les saisons tenues pour indépendantes), p
# par la loi normale avec la correction de continuité, comme trend::smk.test et
# EnvStats::kendallSeasonalTrendTest ; la pente de Sen saisonnière, la médiane de toutes ces pentes
# (trend::sea.sens.slope, EnvStats), et son intervalle à 95 % d'après Var(S) (intervalle_sen, comme
# Gilbert, 1987) ; le τ d'EnvStats : la moyenne des τ des saisons, 2 S_g / (n_g(n_g − 1)),
# pondérée par leurs nombres d'années n_g. Pas de test sous 4 valeurs (après les médianes) dans les
# saisons qui comptent, ni quand chacune garde la même valeur d'une année à l'autre.
kendall_saisonnier <- function(t, y, saison) {
  annee <- floor(round(t, 8))
  g <- factor(paste(saison, annee, sep = "\r"), levels = unique(paste(saison, annee, sep = "\r")))
  premiers <- !duplicated(g)
  v <- as.numeric(tapply(y, g, stats::median))  # (dans l'ordre des niveaux de g : celui des premiers)
  s <- saison[premiers]
  a <- annee[premiers]
  morceaux <- Filter(Negate(is.null), lapply(split(seq_along(v), factor(s, levels = unique(s))), function(k) {
    k <- k[order(a[k])]
    n <- length(k)
    if (n < 2) return(NULL)
    d <- outer(v[k], v[k], "-")
    paires <- lower.tri(d)
    liens <- rle(sort(v[k]))$lengths
    list(n = n, S = sum(sign(d[paires])), var_S = (n * (n - 1) * (2 * n + 5) - sum(liens * (liens - 1) * (2 * liens + 5))) / 18,
         pentes = d[paires] / outer(a[k], a[k], "-")[paires])
  }))
  de <- function(x) unlist(lapply(morceaux, `[[`, x))
  valeurs <- sum(de("n"))
  S <- sum(de("S"))
  var_S <- sum(de("var_S"))
  r <- list(pente = NA_real_, bas = NA_real_, haut = NA_real_, tau = NA_real_, S = NA_real_, var_S = NA_real_, p = NA_real_,
            n_n_etoile = NA_real_, saisons = length(morceaux), medianes = sum(table(g) > 1),
            non_teste = if (valeurs < 4) sprintf(traduire("trop peu d'années par saison : %d valeur(s) dans les saisons vues au moins 2 années, il en faut au moins 4",
                                                          "too few years per season: %d value(s) in the seasons seen in at least 2 years, at least 4 are needed"), valeurs)
                        else if (var_S == 0) traduire("la même valeur chaque année dans chaque saison", "the same value every year in each season")
                        else "")
  if (nzchar(r$non_teste)) return(r)
  pentes <- sort(de("pentes"))
  r[c("pente", "tau", "S", "var_S", "p")] <- list(stats::median(pentes), sum(de("n") * 2 * de("S") / (de("n") * (de("n") - 1))) / valeurs, S, var_S,
                                                  2 * stats::pnorm(-abs((S - sign(S)) / sqrt(var_S))))
  r[c("bas", "haut")] <- as.list(intervalle_sen(pentes, var_S))
  r
}

# L'intervalle à 95 % d'une pente de Sen (Gilbert, 1987 ; Hollander et Wolfe, 1999), d'après la
# variance de S du test : les N pentes rangées (`pentes`), la M-ième et la (N + 1 − M)-ième,
# M = (N − C) / 2 arrondi, C = 1,96 √Var(S), comme trend::sens.slope ; sans bornes quand M < 1 (trop
# peu de pentes pour un intervalle à 95 %).
intervalle_sen <- function(pentes, var_S) {
  N <- length(pentes)
  M <- round((N - stats::qnorm(0.975) * sqrt(var_S)) / 2)
  if (M >= 1) c(pentes[M], pentes[N + 1 - M]) else c(NA_real_, NA_real_)
}
