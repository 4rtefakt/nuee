# Vérifie Arrêter (moteur/R/arret.R) : chaque longue boucle du moteur s'arrête quand le fichier d'arrêt
# est là, et la demande qui la faisait rend « arrêtée » (serveur.R : faire_demande), la séance telle
# qu'elle était avant elle (ses projets, ses modèles, ses piles d'annulation). Par les demandes du
# serveur, telles que l'interface les envoie :
#   - les permutations d'une PLS, la sélection par l'AIC, la simulation de la puissance, la
#     validation croisée d'une PLS (le nombre de composantes choisi), d'une OPLS, d'un modèle linéaire
#     et d'un GAM, un modèle par réponse, iPLS, le choix de k des k-moyennes ;
#   - les départs d'un NMDS, les permutations de la PERMANOVA (et de la dispersion), du test de Mantel
#     (et de son corrélogramme), des espèces indicatrices, des tests d'une RDA, des variables ajustées
#     aux axes d'une PCoA, de l'I de Moran ;
#   - la validation croisée d'une forêt aléatoire et d'un arbre, les arbres boostés, la stabilité d'un
#     arbre, la sélection pas à pas des points de changement de TRIM ;
#   - les étapes rejouées d'une mise à jour des données ;
#   - une demande qui avait changé la séance avant d'être arrêtée : tout est remis ; sans demande
#     pour le recevoir, une erreur de classe calcul_arrete ; sans le fichier, rien ne s'arrête ;
#   - l'export vers R d'une forêt, d'arbres boostés, de TRIM et d'un NMDS : sans verifier_arret.
#   Rscript moteur/tests/verifier_arret.R

options(warn = 1, stringsAsFactors = FALSE)
racine_nuee <- normalizePath(".", winslash = "/")
# Le serveur sans son écoute (comme verifier_etapes.R) : chaque expression de serveur.R jusqu'à
# l'interface HTTP, sa racine et ses arguments donnés ici.
local({
  assign("racine", racine_nuee, globalenv())
  for (e in parse(file.path(racine_nuee, "moteur", "serveur.R"), encoding = "UTF-8", keep.source = FALSE)) {
    texte <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
    if (startsWith(texte, "appli <- list(")) break
    if (startsWith(texte, "local(") && grepl("racine <<-", texte, fixed = TRUE)) next
    if (startsWith(texte, "args <- commandArgs")) {
      assign("args", c("8799", file.path(racine_nuee, "app", "interface")), globalenv())
      next
    }
    eval(e, globalenv())
  }
})
options(nuee.langue = "fr")

echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
dossier <- tempfile("nuee_arret")
dir.create(dossier)
arret <- file.path(dossier, "arreter")
options(nuee.arret = arret)

# Une demande comme l'interface l'envoie : `q` passé par le JSON, la réponse mise en JSON.
json <- function(q) jsonlite::fromJSON(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA)), simplifyVector = TRUE)
demande <- function(nom, q = list()) {
  r <- DEMANDES[[nom]](json(q))
  repondre(200L, r)
  invisible(r)
}
seance <- function() mget(setdiff(ls(etat, all.names = TRUE), c("journal", "n_journal")), envir = etat)
# La demande `nom`, le fichier d'arrêt posé avant elle (la première vérification le regarde) : arrêtée,
# la séance inchangée, en combien de secondes.
stoppe <- function(nom, q) {
  assign("dernier", -1, envir = environment(verifier_arret))
  writeLines("1", arret)
  on.exit(unlink(arret))
  avant <- seance()
  debut <- proc.time()[[3]]
  r <- faire_demande(DEMANDES[[nom]], json(q))
  list(arretee = is.null(r), inchangee = identical(seance(), avant), duree = proc.time()[[3]] - debut)
}
ok <- function(s) s$arretee && s$inchangee
dit <- function(quoi, s) sprintf("%s : arrêté%s, la séance inchangée (%.2f s)", quoi, if (s$arretee) "" else " NON", s$duree)
vide <- stats::setNames(list(), character(0))
q_analyse <- function(projet, type, x, y, options, classe = NULL, echelle = "none", nom = "M1", ...) {
  c(list(projet = projet, type = type, x = x, y = y, classe = classe, exclues = list(), test = list(), echelle = echelle,
         echelles = vide, transformations = vide, croises = list(), spectres = list(), A = 0, G = 7, vc = NULL,
         blocs = vide, modificateurs = vide, aleatoires = list(), correlation = NULL, famille = NULL, lisses = list(),
         k_courbes = vide, par_courbes = vide, validation = NULL, decalage = NULL, lisse_spatial = NULL,
         croises_reglages = vide, nom = nom, titre = "", condition = "", options = options), list(...))
}

# ---------------------------------------------------------------- les données

# des relevés : un site, une année, un habitat, une température, une profondeur, une densité, et
# quarante colonnes d'un spectre pour iPLS
set.seed(2)
n <- 120
releves <- data.frame(releve = sprintf("R%03d", 1:n), site = sprintf("S%02d", (1:n - 1) %% 12 + 1), annee = rep(2001:2010, length.out = n),
                      habitat = sample(c("herbier", "sable", "roche"), n, TRUE), temperature = round(stats::rnorm(n, 15, 2.5), 2),
                      profondeur = round(stats::runif(n, 1, 30), 1))
releves$densite <- round(2 + 0.25 * releves$temperature - 0.04 * releves$profondeur + stats::rnorm(n, 0, 0.8), 3)
releves$x <- round(stats::runif(n, 0, 1000))
releves$y <- round(stats::runif(n, 0, 1000))
spectre <- outer(releves$densite, sin(seq(0, 3, length.out = 40))) + matrix(stats::rnorm(n * 40, 0, 0.3), n, 40)
colnames(spectre) <- sprintf("l%03d", 1:40)
f_releves <- file.path(dossier, "releves.csv")
utils::write.csv(cbind(releves, round(spectre, 4)), f_releves, row.names = FALSE)
pid <- demande("ouvrir", list(chemin = f_releves))$id
u <- function(...) as.numeric(numeros(projet(pid), c(...)))

# des communautés : un secteur, un habitat, une température, une altitude, 15 espèces
set.seed(4)
nc <- 60
S <- 15
temperature <- round(stats::runif(nc, 5, 25), 1)
altitude <- round(stats::runif(nc, 0, 1500))
habitat <- sample(c("foret", "prairie", "marais"), nc, TRUE)
eta <- 1.5 - outer(temperature, stats::runif(S, 5, 25), "-")^2 / 50 - outer(altitude, stats::runif(S, 0, 1500), "-")^2 / 8e5
Y <- matrix(stats::rpois(nc * S, exp(eta)), nc, S, dimnames = list(NULL, sprintf("sp%02d", 1:S)))
Y[cbind(which(rowSums(Y) == 0), 1)] <- 1
f_communautes <- file.path(dossier, "communautes.csv")
utils::write.csv(data.frame(releve = sprintf("C%03d", 1:nc), secteur = sprintf("Z%d", (1:nc - 1) %% 6 + 1), habitat = habitat,
                            temperature = temperature, altitude = altitude, Y), f_communautes, row.names = FALSE)
cid <- demande("ouvrir", list(chemin = f_communautes))$id
uc <- function(...) as.numeric(numeros(projet(cid), c(...)))
especes <- uc(sprintf("sp%02d", 1:S))
communes <- list(distance = "bray", binaire = FALSE, transformation = "aucune", permutations = 999, graine = 1, strates = NULL)

# des comptages de TRIM : 20 sites, 12 années
set.seed(5)
g <- expand.grid(j = 1:12, i = 1:20)
compte <- stats::rpois(nrow(g), exp(2 + 0.05 * pmin(g$j, 6) - 0.08 * pmax(g$j - 6, 0) + stats::rnorm(20, 0, 0.5)[g$i]))
f_trim <- file.path(dossier, "trim.csv")
utils::write.csv(data.frame(nom = sprintf("r%04d", seq_len(nrow(g))), site = sprintf("S%02d", g$i), annee = 2000 + g$j, compte = compte), f_trim, row.names = FALSE)
tid <- demande("ouvrir", list(chemin = f_trim))$id
ut <- function(...) as.numeric(numeros(projet(tid), c(...)))

# les modèles sur lesquels se font les calculs demandés ensuite
pls <- demande("ajuster", list(projet = pid, type = "PLS", x = as.list(u("temperature", "profondeur", "annee")), y = list(u("densite")), A = 2, nom = "P1"))
lm1 <- demande("ajuster", list(projet = pid, type = "LM", x = as.list(u("temperature", "profondeur", "habitat", "annee")), y = list(u("densite")), nom = "L1"))
lmm <- demande("ajuster", list(projet = pid, type = "LMM", x = list(u("annee")), y = list(u("densite")), nom = "LM1", echelle = "none",
                               aleatoires = list(list(by = "variable", variable = u("site")))))
spec <- demande("ajuster", list(projet = pid, type = "PLS", x = as.list(u(sprintf("l%03d", 1:40))), y = list(u("densite")), A = 2, nom = "SP"))
km <- demande("ajuster", list(projet = pid, type = "KMEANS", x = as.list(u("temperature", "profondeur")), options = list(k = 3), nom = "K1"))
arbre <- demande("ajuster", q_analyse(pid, "TREE", as.list(u("temperature", "profondeur")), list(u("densite")), list(), nom = "A1"))
demande("coordonnees", list(projet = pid, regler = TRUE, x = u("x"), y = u("y"), systeme = "2154"))

# ---------------------------------------------------------------- les boucles

cat("Les modèles de composantes et les régressions\n")
s <- stoppe("permutations", list(modele = pls$id, n = 999))
verifier(dit("les permutations d'une PLS", s), ok(s))
s <- stoppe("selection_aic", list(modele = lm1$id))
verifier(dit("la sélection par l'AIC", s), ok(s))
s <- stoppe("puissance_tendance", list(modele = lmm$id, unite = "pente", tendance = -0.05, simuler = TRUE, simulation = list(annees = 6, unites = 10, n = 500)))
verifier(dit("la simulation de la puissance", s), ok(s))
s <- stoppe("ajuster", list(projet = pid, type = "PLS", x = as.list(u("temperature", "profondeur", "annee")), y = list(u("densite")), A = 0, nom = "P2"))
verifier(dit("la validation croisée d'une PLS (le nombre de composantes choisi)", s), ok(s))
s <- stoppe("ajuster", list(projet = pid, type = "OPLS", x = as.list(u("temperature", "profondeur", "annee")), y = list(u("densite")), A = 2, nom = "O1"))
verifier(dit("la validation croisée d'une OPLS", s), ok(s))
s <- stoppe("ajuster", list(projet = pid, type = "LM", x = as.list(u("temperature", "profondeur")), y = list(u("densite")), nom = "L2"))
verifier(dit("la validation croisée d'un modèle linéaire", s), ok(s))
s <- stoppe("ajuster", list(projet = pid, type = "GAM", x = as.list(u("temperature", "profondeur")), y = list(u("densite")), famille = "gaussienne",
                            lisses = list(u("temperature")), nom = "G1"))
verifier(dit("la validation croisée d'un GAM", s), ok(s))
s <- stoppe("par_reponse", list(modele = lm1$id, reponses = as.list(u("x", "y", "l001", "l002"))))
verifier(dit("un modèle par réponse", s), ok(s))
s <- stoppe("ipls", list(modele = spec$id, n = 10, A = 2))
verifier(dit("iPLS", s), ok(s))
s <- stoppe("kmeans_choix", list(modele = km$id))
verifier(dit("le choix de k des k-moyennes", s), ok(s))

cat("Les communautés et l'espace\n")
s <- stoppe("ajuster", q_analyse(cid, "NMDS", as.list(uc("temperature", "altitude")), as.list(especes), c(communes, list(k = 2, departs = 20)), nom = "N1"))
verifier(dit("les départs d'un NMDS (et ses variables ajustées)", s), ok(s))
s <- stoppe("ajuster", q_analyse(cid, "PCOA", as.list(uc("temperature", "altitude", "habitat")), as.list(especes), c(communes, list(correction = "aucune")), nom = "C1"))
verifier(dit("les variables ajustées aux axes d'une PCoA", s), ok(s))
s <- stoppe("ajuster", q_analyse(cid, "PERMANOVA", as.list(uc("temperature", "habitat")), as.list(especes),
                                 c(communes, list(correction = "aucune", par = "termes", dispersion = "centroide")), nom = "PM1"))
verifier(dit("les permutations de la PERMANOVA", s), ok(s))
s <- stoppe("ajuster", q_analyse(cid, "MANTEL", as.list(uc("temperature", "altitude")), as.list(especes),
                                 c(communes, list(methode = "pearson", classes = 10, z = list(), distance_x = "variables", z_transformations = vide, z_echelles = vide)),
                                 echelle = "UV", nom = "MA1"))
verifier(dit("les permutations du test de Mantel", s), ok(s))
s <- stoppe("ajuster", q_analyse(cid, "INDVAL", list(), as.list(especes), list(indice = "IndVal.g", ordre = NULL, presence = FALSE, permutations = 999, graine = 1,
                                                                             strates = NULL, correction = "BH"), classe = uc("habitat"), nom = "I1"))
verifier(dit("les permutations des espèces indicatrices", s), ok(s))
rda <- demande("ajuster", q_analyse(cid, "RDA", as.list(uc("temperature", "altitude", "habitat")), as.list(especes),
                                    list(transformation = "hellinger", reduire = FALSE, permutations = 199, graine = 1, strates = NULL, z = list(), z_transformations = vide), nom = "R1"))
s <- stoppe("rda_tests", list(modele = rda$id))
verifier(dit("les tests par permutation d'une RDA", s), ok(s))
s <- stoppe("autocorrelation_spatiale", list(projet = pid, variable = u("densite"), permutations = 999, voisinage = list()))
verifier(dit("les permutations de l'I de Moran", s), ok(s))

cat("Les arbres, les forêts, les arbres boostés, TRIM\n")
if (requireNamespace("ranger", quietly = TRUE)) {
  s <- stoppe("ajuster", q_analyse(pid, "FOREST", as.list(u("temperature", "profondeur", "habitat")), list(u("densite")),
                                   list(arbres = 200, mtry = NULL, min_noeud = NULL, fraction = 1, importance = "brute"), nom = "F1"))
  verifier(dit("la validation croisée d'une forêt aléatoire", s), ok(s))
} else cat("  (ranger absent : la forêt n'est pas essayée)\n")
s <- stoppe("ajuster", q_analyse(pid, "BOOST", as.list(u("temperature", "profondeur", "habitat")), list(u("densite")),
                                 list(loi = "gaussienne", arbres_max = 2000, profondeur = 3, min_feuille = 10, taux = 0.01, part = 0.5), nom = "B1"))
verifier(dit("les arbres boostés (et leur validation croisée)", s), ok(s))
s <- stoppe("ajuster", q_analyse(pid, "TREE", as.list(u("temperature", "profondeur")), list(u("densite")), list(), nom = "A2"))
verifier(dit("la validation croisée d'un arbre", s), ok(s))
s <- stoppe("arbre_stabilite", list(modele = arbre$id))
verifier(dit("la stabilité d'un arbre (ses tirages)", s), ok(s))
s <- stoppe("ajuster", q_analyse(tid, "TRIM", list(), list(ut("compte")),
                                 list(site = list(by = "variable", variable = ut("site")), temps = list(by = "variable", variable = ut("annee")), modele = 2,
                                      changements = "tous", pas_a_pas = TRUE, surdispersion = TRUE, correlation = FALSE, poids = NULL, base = NULL), nom = "T1"))
verifier(dit("la sélection pas à pas des points de changement de TRIM", s), ok(s))

cat("Les étapes rejouées d'une mise à jour\n")
demande("modifier_valeur", list(projet = pid, observation = numeros_observations(projet(pid), "R001"), variable = u("temperature"), valeur = "16"))
demande("variable_calculee", list(projet = pid, nom = "chaleur", formule = "temperature * 2"))
s <- stoppe("mettre_a_jour", list(projet = pid))
verifier(dit("une mise à jour des données, ses étapes rejouées", s), ok(s))

cat("La séance remise, les autres façons\n")
change <- function(q) {
  etat$projets[[pid]]$modifie <- FALSE
  etat$projets[[pid]]$nom <- "changé"
  etat$annulations[[pid]] <- NULL
  etat$autre_chose <- 1
  verifier_arret()
  "fini"
}
avant <- seance()
writeLines("1", arret)
assign("dernier", -1, envir = environment(verifier_arret))
r <- faire_demande(change, list())
unlink(arret)
verifier("une demande qui avait changé la séance avant d'être arrêtée : son projet, ses piles, un nouvel élément, tout est remis",
         is.null(r) && identical(seance(), avant) && !exists("autre_chose", envir = etat, inherits = FALSE) && identical(projet(pid)$nom, "releves.csv"))
assign("dernier", -1, envir = environment(verifier_arret))
verifier("sans le fichier, la demande se fait", identical(faire_demande(change, list())$reponse, "fini") && identical(projet(pid)$nom, "changé"))
etat$projets[[pid]]$nom <- "releves.csv"
rm("autre_chose", envir = etat)
writeLines("1", arret)
assign("dernier", -1, envir = environment(verifier_arret))
seul <- tryCatch({ verifier_arret(); "continué" }, calcul_arrete = function(e) conditionMessage(e))
unlink(arret)
verifier("sans demande pour le recevoir (le moteur dans R seul) : une erreur de classe calcul_arrete", identical(seul, "Calcul arrêté."))
# (le fichier n'est regardé que cinq fois par seconde : juste après une vérification, il ne l'est pas)
writeLines("1", arret)
assign("dernier", proc.time()[[3]], envir = environment(verifier_arret))
sitot <- tryCatch({ verifier_arret(); "continué" }, calcul_arrete = function(e) "arrêté")
unlink(arret)
verifier("regardé au plus cinq fois par seconde", identical(sitot, "continué"))

cat("L'export vers R, sans verifier_arret\n")
sans <- function(id, nom) {
  f <- file.path(dossier, paste0(nom, ".R"))
  r <- tryCatch(demande("script_r", list(modele = id, chemin = f)), error = function(e) NULL)
  !is.null(r) && file.exists(f) && !any(grepl("verifier_arret", readLines(f, encoding = "UTF-8", warn = FALSE), fixed = TRUE)) && is.null(r$ecart)
}
b1 <- demande("ajuster", q_analyse(pid, "BOOST", as.list(u("temperature", "profondeur")), list(u("densite")),
                                   list(loi = "gaussienne", arbres_max = 300, profondeur = 2, min_feuille = 10, taux = 0.05, part = 0.5), nom = "B2"))
t1 <- demande("ajuster", q_analyse(tid, "TRIM", list(), list(ut("compte")),
                                   list(site = list(by = "variable", variable = ut("site")), temps = list(by = "variable", variable = ut("annee")), modele = 2,
                                        changements = "tous", pas_a_pas = TRUE, surdispersion = TRUE, correlation = FALSE, poids = NULL, base = NULL), nom = "T2"))
n1 <- demande("ajuster", q_analyse(cid, "NMDS", list(), as.list(especes), utils::modifyList(communes, list(permutations = 0, k = 2, departs = 5)), nom = "N2"))
verifier("arbres boostés, TRIM pas à pas, NMDS : leur script n'appelle pas verifier_arret, et redonne leurs résultats",
         sans(b1$id, "boost") && sans(t1$id, "trim") && sans(n1$id, "nmds"))
if (requireNamespace("ranger", quietly = TRUE)) {
  f1 <- demande("ajuster", q_analyse(pid, "FOREST", as.list(u("temperature", "profondeur")), list(u("densite")),
                                     list(arbres = 100, mtry = NULL, min_noeud = NULL, fraction = 1, importance = "brute"), nom = "F2"))
  verifier("une forêt aléatoire : son script n'appelle pas verifier_arret, et redonne ses résultats", sans(f1$id, "foret"))
}

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
