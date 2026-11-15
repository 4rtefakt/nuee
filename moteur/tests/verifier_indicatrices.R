# Vérifie les espèces indicatrices (moteur/R/indicatrices.R) : contre des calculs directs (A, B
# et IndVal écrits boucle par boucle, le test par une boucle sur les mêmes permutations,
# p.adjust) et, quand il est installé, contre indicspecies::multipatt (IndVal.g et IndVal, les
# groupes seuls ou leurs combinaisons) : mêmes statistiques, mêmes groupes et, pour les mêmes
# permutations (passées à multipatt), mêmes valeurs p. Puis ce que l'interface en reçoit, le
# texte des méthodes, les tableaux exportés, les refus.
#   Rscript moteur/tests/verifier_indicatrices.R
# (NUEE_INDICSPECIES : une bibliothèque où indicspecies est installé)

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-10) isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), tolerance = tol, check.attributes = FALSE))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

if (nzchar(Sys.getenv("NUEE_INDICSPECIES"))) .libPaths(c(Sys.getenv("NUEE_INDICSPECIES"), .libPaths()))
avec_indicspecies <- requireNamespace("indicspecies", quietly = TRUE)
cat(if (avec_indicspecies) sprintf("(indicspecies %s installé : comparaisons avec lui)\n", utils::packageVersion("indicspecies")) else "(indicspecies absent : calculs directs seulement)\n")

# ---------------------------------------------------------------- les données

# 36 relevés en trois milieux d'effectifs inégaux (forêt 10, prairie 12, marais 14), dans 6 sites ;
# 10 espèces de préférences diverses (sp10 partout, abondante), sp11 absente de tous
set.seed(7)
milieu <- factor(rep(c("foret", "prairie", "marais"), c(10, 12, 14)), levels = c("foret", "prairie", "marais"))
n <- length(milieu)
pref <- rbind(foret = c(4, 0.2, 0.2, 2, 2, 0.1, 1, 0.5, 0.3, 5),
              prairie = c(0.2, 4, 0.2, 2, 0.1, 2, 1, 0.5, 0.1, 5),
              marais = c(0.2, 0.2, 4, 0.1, 2, 2, 1, 0.5, 0.6, 5))
mu <- pref[as.character(milieu), ]
ab <- matrix(stats::rpois(length(mu), mu), n, 10, dimnames = list(NULL, sprintf("sp%02d", 1:10)))
ab[, 10] <- pmax(ab[, 10], 1)  # (présente partout)
ab <- cbind(ab, sp11 = 0)
site <- rep(sprintf("S%d", 1:6), length.out = n)
df <- data.frame(nom = sprintf("r%02d", 1:n), milieu = milieu, site = site, ab, stringsAsFactors = FALSE)
df$sp03[5] <- NA  # (un relevé laissé de côté)
p <- projet_depuis_tableau(df, "indicatrices")
u <- function(...) numeros(p, c(...))
especes <- colnames(ab)
garde <- seq_len(n) != 5
X <- ab[garde, 1:10]
g <- milieu[garde]
B <- 199

# les statistiques écrites boucle par boucle : pour une combinaison de groupes `cg`
directes <- function(X, g, cg, egalises) {
  dans <- g %in% cg
  sapply(seq_len(ncol(X)), function(j) {
    x <- X[, j]
    A <- if (egalises) {
      moy <- tapply(x, g, mean)
      sum(moy[cg]) / sum(moy)
    } else sum(x[dans]) / sum(x)
    Bj <- mean(x[dans] > 0)
    c(A = A, B = Bj, IV = sqrt(A * Bj))
  })
}

cat("Calculs directs\n")
d1 <- definir_modele(p, "INDVAL", x = NULL, y = u(especes), classe = u("milieu"), echelle = "none",
                     options = list(permutations = B, graine = 3, ordre = 1))
m1 <- ajuster(p, d1)
verifier(sprintf("%d relevés (celui où une valeur manque laissé de côté), 10 espèces (sp11, absente, retirée)", m1$N),
         m1$N == 35 && identical(m1$exclues, 5) && identical(m1$absentes, "sp11") && length(m1$especes) == 10)
ok <- TRUE
for (k in seq_along(levels(milieu))) {
  s <- directes(X, g, levels(milieu)[k], TRUE)
  ok <- ok && proche(m1$A[k, ], s["A", ]) && proche(m1$B[k, ], s["B", ]) && proche(m1$IndVal[k, ], s["IV", ])
}
verifier("IndVal.g, groupes seuls : A (moyennes des groupes), B et racine(A × B) de chaque groupe = boucles", ok)
verifier("le groupe de chaque espèce : sa plus grande valeur", identical(m1$meilleure, unname(apply(m1$IndVal, 2, which.max))) &&
           identical(m1$niveaux[m1$meilleure][1:3], c("foret", "prairie", "marais")))
P <- permutations_lignes(m1$N, B, NULL, 3)
plus_grande <- function(gp) max_par <- apply(sapply(levels(milieu), function(h) directes(X, gp, h, TRUE)["IV", ]), 1, max)
obs <- plus_grande(g)
compte <- rowSums(sapply(seq_len(B), function(b) plus_grande(g[P[b, ]]) >= obs - sqrt(.Machine$double.eps)))
p_dir <- (compte + 1) / (B + 1)
verifier(sprintf("p par permutation (sp01 : %.3f) = boucle sur les mêmes permutations des groupes", m1$p[1]), proche(m1$p, p_dir))
verifier("p corrigées (Benjamini–Hochberg) = p.adjust", proche(m1$p_ajuste, stats::p.adjust(p_dir, "BH")))
d2 <- definir_modele(p, "INDVAL", x = NULL, y = u(especes), classe = u("milieu"), echelle = "none",
                     options = list(permutations = B, graine = 3, indice = "IndVal", correction = "holm"))
m2 <- ajuster(p, d2)
C <- m2$combinaisons
verifier("combinaisons de 1 à 3 groupes : 7, dans l'ordre de combn, nommées « a+b »",
         ncol(C) == 7 && identical(colnames(C), c("foret", "prairie", "marais", "foret+prairie", "foret+marais", "prairie+marais", "foret+prairie+marais")))
ok <- TRUE
for (j in seq_len(ncol(C))) {
  s <- directes(X, g, levels(milieu)[C[, j] == 1], FALSE)
  ok <- ok && proche(m2$A[j, ], s["A", ]) && proche(m2$B[j, ], s["B", ]) && proche(m2$IndVal[j, ], s["IV", ])
}
verifier("IndVal (sommes), combinaisons : A, B et IndVal de chaque combinaison = boucles", ok)
verifier("une espèce présente partout (sp10) : sa combinaison est celle des trois groupes, sans p", m2$meilleure[10] == 7 && is.na(m2$p[10]) && is.na(m2$p_ajuste[10]))
verifier("p corrigées (Holm) sur les espèces testées seulement", proche(m2$p_ajuste[-10], stats::p.adjust(m2$p[-10], "holm")))
d3 <- definir_modele(p, "INDVAL", x = NULL, y = u(especes), classe = u("milieu"), echelle = "none",
                     options = list(permutations = 99, graine = 1, ordre = 2, presence = TRUE, strates = list(by = "variable", variable = u("site"))))
m3 <- ajuster(p, d3)
P3 <- permutations_lignes(m3$N, 99, site[garde], 1)
verifier("présences seules : A et B des présences (0/1) ; combinaisons jusqu'à l'ordre 2 (6)", ncol(m3$combinaisons) == 6 &&
           proche(m3$IndVal[4, ], directes((X > 0) * 1, g, c("foret", "prairie"), TRUE)["IV", ]))
verifier("permutations à l'intérieur des sites ; les mêmes pour la même graine, le hasard de la session intact", {
  set.seed(99); a <- stats::runif(1); set.seed(99)
  m3b <- ajuster(p, d3); b <- stats::runif(1)
  all(apply(P3, 1, function(ip) all(site[garde][ip] == site[garde]))) && identical(m3b$p, m3$p) && identical(a, b)
})

if (avec_indicspecies) {
  cat("Comparaison avec indicspecies::multipatt\n")
  cl <- as.character(g)
  compare <- function(m, mp, quoi) {
    # (multipatt range les groupes dans l'ordre alphabétique : ses combinaisons, remises dans l'ordre de Nuée)
    j <- match(colnames(m$combinaisons), colnames(mp$str))
    if (anyNA(j)) j <- match(vapply(strsplit(colnames(m$combinaisons), "+", fixed = TRUE), function(x) paste(sort(x), collapse = "+"), ""), colnames(mp$str))
    s <- mp$sign
    meme_groupe <- colnames(mp$str)[s$index] == colnames(mp$str)[j[m$meilleure]]
    # (deux combinaisons à égalité, comme sp10 des présences : multipatt tire l'une au hasard
    # (max.col), Nuée prend la première ; la valeur et p sont les mêmes)
    egalites <- apply(mp$str, 1, function(v) sum(v >= max(v) - 1e-12) > 1)
    verifier(sprintf("%s : statistiques, A, B de chaque combinaison ; groupe%s, valeur et p de chaque espèce = multipatt", quoi,
                     if (any(egalites)) sprintf(" (sauf %s, à égalité entre combinaisons)", paste(rownames(mp$str)[egalites], collapse = ", ")) else ""),
             !anyNA(j) && proche(t(m$IndVal), mp$str[, j]) && proche(t(m$A), mp$A[, j]) && proche(t(m$B), mp$B[, j]) && all(meme_groupe[!egalites]) &&
               proche(m$valeur, s$stat) && isTRUE(all.equal(m$p, s$p.value)))
  }
  compare(m1, indicspecies::multipatt(X, cl, func = "IndVal.g", duleg = TRUE, control = NULL, permutations = P), "IndVal.g, groupes seuls (duleg = TRUE)")
  P2 <- permutations_lignes(m2$N, B, NULL, 3)
  compare(m2, indicspecies::multipatt(X, cl, func = "IndVal", control = NULL, permutations = P2), "IndVal, toutes les combinaisons")
  compare(m3, indicspecies::multipatt((X > 0) * 1, cl, func = "IndVal.g", max.order = 2, control = NULL, permutations = P3),
          "IndVal.g des présences, jusqu'à l'ordre 2, permutations par site")
  d4 <- definir_modele(p, "INDVAL", x = NULL, y = u(especes), classe = u("milieu"), echelle = "none", options = list(permutations = 999, graine = 8))
  m4 <- ajuster(p, d4)
  compare(m4, indicspecies::multipatt(X, cl, func = "IndVal.g", control = NULL, permutations = permutations_lignes(m4$N, 999, NULL, 8)),
          "IndVal.g (défaut), toutes les combinaisons, 999 permutations")
}

cat("Ce que l'interface reçoit, texte des méthodes, tableaux, refus\n")
etat <- new.env()
etat$projets <- list(P1 = c(p, list(id = "P1")))
class(etat$projets$P1) <- class(p)
etat$modeles <- list()
projet <- function(id) etat$projets[[id]]
modele <- function(id) etat$modeles[[id]]
garder <- function(id, d, m) { etat$modeles[[id]] <- list(projet = "P1", definition = d, modele = m); id }
json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA, force = TRUE)
resultat <- function(id) { force(id); fonction_analyse("resultat_", etat$modeles[[id]]$definition$type)(id) }
r <- resultat(garder("M1", d2, m2))
verifier("analyse sans prédiction ; le tableau des espèces (groupe, A, B, IndVal, p, p corrigée, une colonne par groupe), les statistiques de chaque combinaison",
         isTRUE(r$analyse) && identical(r$prediction, FALSE) && identical(as.character(r$vues_defaut), c("indicatrices", "indval_graphe")) &&
           nrow(r$especes) == 10 && all(c("groupe", "A", "B", "indval", "p", "p_ajuste", "s.foret", "s.marais") %in% names(r$especes)) &&
           ncol(r$indval) == 8 && nrow(r$observations) == 35 && r$retenues$corrigees == sum(m2$p_ajuste <= 0.05, na.rm = TRUE) && inherits(json(r), "json"))
verifier("ses réglages pour Modifier le modèle", identical(r$reglages$options$indice, "IndVal") && identical(r$reglages$classe, u("milieu")))
for (langue in c("fr", "en")) {
  t1 <- texte_methodes(p, d1, m1, langue, "essai")
  t2 <- texte_methodes(p, d2, m2, langue, "essai")
  t3 <- texte_methodes(p, d3, m3, langue, "essai")
  verifier(sprintf("texte des méthodes (%s) : IndVal.g ou IndVal, combinaisons, permutations (par site), correction ; références", langue),
           any(grepl("IndVal.g", t1$paragraphes)) && any(grepl("Benjamini", t1$paragraphes)) && any(grepl("Holm", t2$paragraphes)) &&
             any(grepl("De Cáceres et al., 2010", t2$paragraphes)) && !any(grepl("De Cáceres et al., 2010", t1$paragraphes)) &&
             any(grepl(if (langue == "fr") "à l'intérieur de chaque site" else "within each site", t3$paragraphes)) &&
             any(grepl("Dufrêne, M., & Legendre, P. (1997)", t1$references, fixed = TRUE)) && any(grepl("Holm, S.", t2$references, fixed = TRUE)) &&
             !any(grepl("SIMCA", c(t1$paragraphes, t2$paragraphes))))
  verifier(sprintf("texte des méthodes (%s) : au singulier, le relevé laissé de côté et l'espèce absente", langue),
           any(grepl(if (langue == "fr") "(une autre laissée de côté :" else "(one other left out:", t1$paragraphes, fixed = TRUE)) &&
             any(grepl(if (langue == "fr") "sp11, absente de ces observations, en a été retirée" else "sp11, absent from these observations, was left out", t1$paragraphes, fixed = TRUE)))
}
tb <- tables_resultats(p, m2)
verifier("tableaux exportés : les espèces, le résumé, les groupes, IndVal, A et B de chaque combinaison", all(vapply(tb, is.data.frame, TRUE)) &&
           identical(names(tb), c("especes", "resume", "groupes", "indval", "A", "B")) && nrow(tb$indval) == 10)
neg <- p
neg$donnees[1, as.character(u("sp01"))] <- -1
verifier("refus : sans espèces, sans groupes, avec des X, une valeur négative, une espèce qualitative, un seul groupe",
         nzchar(refus(ajuster(p, definir_modele(p, "INDVAL", x = NULL, y = NULL, classe = u("milieu"), echelle = "none")))) &&
           grepl("Classe", refus(ajuster(p, definir_modele(p, "INDVAL", x = NULL, y = u(especes), echelle = "none")))) &&
           grepl("X", refus(ajuster(p, definir_modele(p, "INDVAL", x = u("sp01"), y = u(especes[-1]), classe = u("milieu"), echelle = "none")))) &&
           grepl("positives", refus(ajuster(neg, definir_modele(neg, "INDVAL", x = NULL, y = u(especes), classe = u("milieu"), echelle = "none")))) &&
           grepl("site", refus(ajuster(p, definir_modele(p, "INDVAL", x = NULL, y = u("site", "sp01"), classe = u("milieu"), echelle = "none")))) &&
           grepl("deux groupes", refus(ajuster(p, definir_modele(p, "INDVAL", x = NULL, y = u(especes), classe = u("milieu"), echelle = "none",
                                                                 observations = which(milieu == "foret"))))))
verifier("refus : un jeu de test", grepl("jeu de test", refus(definir_modele(p, "INDVAL", x = NULL, y = u(especes), classe = u("milieu"), echelle = "none", test = 1:5))))
d0 <- definir_modele(p, "INDVAL", x = NULL, y = u(especes), classe = u("milieu"), echelle = "none", options = list(permutations = 0))
m0 <- ajuster(p, d0)
verifier("sans permutation : aucune p, le texte le dit", all(is.na(m0$p)) && any(grepl("Aucun test", texte_methodes(p, d0, m0, "fr")$paragraphes)))

cat("Export vers R : scripts et rapport, lancés dans une session neuve\n")
dossier <- tempfile("indicatrices")
dir.create(dossier)
rscript <- file.path(R.home("bin"), "Rscript")
# lance un script dans une session neuve (sans le moteur, ses graphiques sans fichier), depuis un
# autre dossier, et garde les objets nommés
lanceur <- file.path(dossier, "lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".r <- mget(strsplit(.a[3], \",\")[[1]], ifnotfound = list(NULL))", "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, objets) {
  sortie <- tempfile("sortie", dossier, ".rds")
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), shQuote(paste(objets, collapse = ","))),
                                       stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
d5 <- definir_modele(p, "INDVAL", x = NULL, y = u(especes), classe = u("milieu"), echelle = "none", condition = 'site != "S6"',
                     options = list(permutations = 99, graine = 5, ordre = 2, strates = list(by = "variable", variable = u("site"))))
cas <- list(list(id = "indval_g", langue = "fr", quoi = "IndVal.g, groupes seuls", d = d1), list(id = "indval_combinaisons", langue = "en", quoi = "IndVal, toutes les combinaisons, Holm", d = d2),
            list(id = "indval_presences", langue = "fr", quoi = "présences, ordre 2, permutations par site", d = d3), list(id = "indval_sans_test", langue = "en", quoi = "sans permutation", d = d0),
            list(id = "indval_condition", langue = "en", quoi = "une condition, ordre 2, par site", d = d5))
for (k in cas) {
  m <- ajuster(p, k$d)
  r <- tryCatch(ecrire_script(p, k$d, file.path(dossier, paste0(k$id, ".R")), "moteur", m = m, version = "essai", langue = k$langue), error = function(e) e)
  if (inherits(r, "error")) {
    verifier(sprintf("%s (%s) : écrit", k$id, k$quoi), FALSE)
    cat("    erreur :", conditionMessage(r), "\n")
    next
  }
  n <- function(x) nom_communautes(x, k$langue)
  s <- lancer(r$script, n(c("statistiques", "meilleures", "p", "p_ajuste", "especes")))
  st <- s[[n("statistiques")]]
  texte <- readLines(r$script, encoding = "UTF-8")
  code <- sub("#.*$", "", texte)
  memes <- length(s) > 0 && proche(st$A, m$A) && proche(st$B, m$B) && proche(st$IndVal, m$IndVal) &&
    identical(as.integer(s[[n("meilleures")]][[n("rang")]]), as.integer(m$meilleure)) && proche(s[[n("meilleures")]][[n("valeur")]], m$valeur) &&
    isTRUE(all.equal(s[[n("p")]], m$p)) && isTRUE(all.equal(s[[n("p_ajuste")]], m$p_ajuste)) &&
    identical(s[[n("especes")]][[n("groupe")]], table_indval(m)$groupe)
  verifier(sprintf("%s (%s, %s) : sans écart vu par le moteur ; lancé seul, mêmes A, B, valeurs, groupes et p ; R seul, sans indicspecies", k$id, k$quoi, k$langue),
           is.null(r$ecart) && memes && !any(grepl("library(", code, fixed = TRUE)) && !any(grepl("indicspecies::", code, fixed = TRUE)) &&
             (k$langue == "fr" || (!any(grepl("statistiques_indval|meilleures_indval|combinaisons_groupes", texte)) && !any(grepl("^ *#(fr|en)( |$)", texte)))))
  if (!is.null(r$ecart) || !isTRUE(memes)) cat("    écart :", r$ecart, "/", utils::tail(attr(s, "messages"), 3), "\n")
}
texte <- readLines(file.path(dossier, "indval_g.R"), encoding = "UTF-8")
verifier("le script reprend les fonctions du moteur telles qu'il les écrit (statistiques_indval, test_indval)",
         all(code_definition_moteur("statistiques_indval", "fr") %in% texte) && all(code_definition_moteur("test_indval", "fr") %in% texte))
# le rapport : méthodes, code, résultats, références ; compilé avec knitr s'il est installé
dir.create(ici <- file.path(dossier, "rapport"))
r <- ecrire_script(p, d3, file.path(ici, "rapport.Rmd"), "moteur", m = m3, version = "essai", langue = "en", donnees = "csv", rdata = TRUE)
texte <- readLines(r$script, encoding = "UTF-8")
if (!is.null(r$ecart)) cat("    écart :", r$ecart, "\n")
debuts <- grep("^```\\{r", texte)
verifier("rapport R Markdown (en) : méthodes, code, tableau des espèces, références ; données en CSV et .RData à côté",
         is.null(r$ecart) && any(grepl("^```\\{r modele\\}", texte)) && any(grepl("^```\\{r especes-table", texte)) && any(grepl("^## Methods", texte)) &&
           any(grepl("^- .*De Cáceres", texte)) && length(r$chemins) == 3 && all(file.exists(r$chemins)))
if (requireNamespace("knitr", quietly = TRUE)) {
  compiler <- file.path(dossier, "compiler.R")
  writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), "a <- commandArgs(TRUE)", "setwd(dirname(a[1]))",
               "knitr::knit(basename(a[1]), output = sub('[.][Rq]md$', '.md', basename(a[1])), quiet = TRUE, envir = new.env())"), compiler)
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(compiler), shQuote(r$script)), stdout = TRUE, stderr = TRUE))
  md <- file.path(ici, "rapport.md")
  verifier("rapport R Markdown : se compile avec knitr, avec son tableau et son graphique",
           file.exists(md) && !any(grepl("^## Error|Error in", readLines(md, encoding = "UTF-8"))) && length(list.files(file.path(ici, "figure"), "\\.png$")) >= 1)
} else {  # sans knitr : le code de ses blocs R tourne dans une session neuve (sans les tableaux de knitr::kable)
  fins <- vapply(debuts, function(j) j + match("```", texte[-seq_len(j)]), 0)
  code <- unlist(lapply(seq_along(debuts), function(j) texte[seq(debuts[j] + 1, length.out = fins[j] - debuts[j] - 1)]))
  writeLines(enc2utf8(c("pdf(NULL)", code[!grepl("knitr::", code)])), file.path(ici, "blocs.R"), useBytes = TRUE)
  avant <- setwd(ici)
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(file.path(ici, "blocs.R"))), stdout = TRUE, stderr = TRUE))
  setwd(avant)
  verifier("rapport R Markdown : le code de ses blocs R tourne (knitr n'est pas installé : pas compilé)", is.null(attr(messages, "status")))
}
unlink(dossier, recursive = TRUE)

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
