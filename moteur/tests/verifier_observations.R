# Vérifie les observations d'un modèle choisies par une condition (moteur/R/condition.R :
# Nouveau modèle › Observations, « Garder seulement les observations où… ») : la condition donne
# les observations que R lui-même donne (with() sur les mêmes données, les qualitatives en texte,
# les identifiants par leur nom, une valeur manquante comptant pour fausse) ; chaque type de modèle
# (ACP, PLS, PLS-DA, OPLS-DA, modèles de classe, LM, LMM, GLM, GAM, arbre, LDA, k-moyennes, RDA,
# CA) ajusté avec elle est celui des mêmes observations cochées à la main, et les régressions
# celles de lm(), nlme::lme(), glm() et mgcv::gam() sur ces lignes ; les refus (un nom inconnu, une
# fonction qui n'est pas permise, jamais exécutée, même d'un fichier reçu ; un résultat de la
# mauvaise longueur ou du mauvais type ; aucune observation) ; les cases cochées gardées ; les
# réajustements (les données changées, la sélection par l'AIC, CAR1, l'ajustement automatique) ;
# l'enregistrement et la relecture ; le texte des méthodes ; la prédiction des observations écartées.
#   Rscript moteur/tests/verifier_observations.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-8) isTRUE(all.equal(unname(a), unname(b), tolerance = tol, check.attributes = FALSE))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
p <- lire_projet("exemple/vergers.json")
obs <- numeros_observations(p)
u <- function(...) numeros(p, c(...))
# les données comme R les lit : les variables qualitatives en texte
valeurs <- function(projet, nom) {
  v <- variable_du_projet(projet, numeros(projet, nom))
  x <- colonne(projet, v$uid, numeros_observations(projet))
  if (length(v$categories)) as.character(v$categories)[x] else x
}
df <- as.data.frame(stats::setNames(lapply(c("variete", "culture", "verger", "poids", "fermete", "sucre", "acidite", "polyphenols", "calcium", "note_gout"),
                                           function(n) valeurs(p, n)),
                                    c("variete", "culture", "verger", "poids", "fermete", "sucre", "acidite", "polyphenols", "calcium", "note_gout")),
                    stringsAsFactors = FALSE)
garde <- function(x) obs[!is.na(x) & x]  # (une valeur manquante compte pour fausse)

cat("La condition, évaluée comme R l'évalue\n")
for (k in list(list('variete == "Gala" & sucre > 12', with(df, variete == "Gala" & sucre > 12)),
               list('verger %in% c("A", "C") | poids >= 170', with(df, verger %in% c("A", "C") | poids >= 170)),
               list("!is.na(polyphenols)", !is.na(df$polyphenols)),
               list("polyphenols > 5", df$polyphenols > 5),
               list("ifelse(culture == \"bio\", sucre > 11, acidite < 6)", with(df, ifelse(culture == "bio", sucre > 11, acidite < 6))),
               list("(sucre > median(sucre)) * 1", as.numeric(df$sucre > stats::median(df$sucre)) == 1),
               list("moyenne_groupe(sucre, verger) > 12.5 & ecart_groupe(poids, variete) > 0",
                    stats::ave(df$sucre, df$verger) > 12.5 & df$poids - stats::ave(df$poids, df$variete) > 0),
               list("startsWith(variete, \"G\") & nchar(verger) == 1 & round(sucre) %in% 11:13", with(df, startsWith(variete, "G") & nchar(verger) == 1 & round(sucre) %in% 11:13)))) {
  v <- valeurs_condition(p, k[[1]])
  attendu <- k[[2]]
  attendu[is.na(attendu)] <- FALSE
  verifier(sprintf("%s : les %d observations que with() donne%s", k[[1]], sum(attendu), if (anyNA(k[[2]])) sprintf(", %d manquantes comptées fausses", sum(is.na(k[[2]]))) else ""),
           identical(as.vector(v), as.vector(attendu)))
}
q <- p
k <- which(vapply(q$variables, function(v) identical(v$ids[1], "sucre"), TRUE))
q$variables[[k]]$ids[1] <- "sucre total (°Brix)"
verifier("un nom avec des espaces et des signes entre `accents graves`",
         identical(as.vector(valeurs_condition(q, "`sucre total (°Brix)` > 12")), df$sucre > 12))

# des sites (un identifiant des observations), deux secteurs par site, huit années (une manque),
# des comptages, des valeurs manquantes
set.seed(5)
n <- 96
site <- rep(sprintf("S%d", 1:6), each = 16)
tr <- data.frame(nom = sprintf("O%03d", 1:n), secteur = rep(rep(c("a", "b"), each = 8), 6), annee = rep(c(2001:2005, 2007:2009), 12),
                 x1 = stats::rnorm(n), x2 = stats::runif(n), stringsAsFactors = FALSE)
effet <- rep(stats::rnorm(6, 0, 0.6), each = 16)
tr$hauteur <- 1 + 0.6 * tr$x1 - 0.8 * tr$x2 + effet + stats::rnorm(n, 0, 0.4)
tr$compte <- stats::rpois(n, exp(0.5 + 0.4 * tr$x1 + effet / 2))
tr$x1[c(7, 40)] <- NA
pr <- projet_depuis_tableau(tr, "terrain")
pr$observation_id_names <- c("Observation", "site")
for (i in seq_along(pr$observations)) pr$observations[[i]]$ids <- c(pr$observations[[i]]$ids[1], site[i])
obr <- numeros_observations(pr)
ur <- function(...) numeros(pr, c(...))
tr$site <- site
verifier("un identifiant des observations par son nom (site), précédent et suivant d'une année dans le même secteur d'un site",
         identical(as.vector(valeurs_condition(pr, 'site != "S2" & annee >= 2003')), with(tr, site != "S2" & annee >= 2003)) &&
           identical(as.vector(valeurs_condition(pr, "!is.na(precedent(hauteur, annee, interaction(site, secteur)))")),
                     with(tr, (paste(site, secteur, annee - 1) %in% paste(site, secteur, annee)))))

cat("Chaque type de modèle : celui des mêmes observations cochées à la main\n")
x <- u("poids", "fermete", "sucre", "acidite")
cond <- 'variete %in% c("Gala", "Golden") | sucre > 13'
choisies <- garde(with(df, variete %in% c("Gala", "Golden") | sucre > 13))
memes_composantes <- function(a, b) proche(a$T, b$T) && proche(a$P, b$P) && proche(a$statistiques$composantes, b$statistiques$composantes)
for (type in c("PCA", "PLS", "OPLS")) {
  d <- definir_modele(p, type, x = x, y = if (type != "PCA") u("note_gout"), A = 2, condition = cond)
  a <- ajuster(p, d)
  b <- ajuster(p, definir_modele(p, type, x = x, y = if (type != "PCA") u("note_gout"), A = 2, observations = choisies))
  verifier(sprintf("%s : ses %d observations, ses scores, loadings, R2 et Q2 = ceux des observations cochées à la main", type, length(choisies)),
           identical(a$observations, choisies) && identical(as.numeric(d$cochees), obs) && memes_composantes(a, b))
}
# PLS-DA : ses classes connues d'abord, puis la condition ; OPLS-DA : ses composantes prédictives
# d'après les classes qui restent (une de moins que de classes)
pq <- p
pq$donnees[c(3, 9), as.character(u("variete"))] <- NA  # deux pommes sans variété
dq <- definir_modele(pq, "PLS-DA", x = x, classe = u("variete"), A = 2, condition = 'variete != "Reinette"')
aq <- ajuster(pq, dq)
attendu <- setdiff(garde(df$variete != "Reinette"), obs[c(3, 9)])
verifier("PLS-DA : les observations de classe connue, puis celles où la condition est vraie ; les mêmes résultats qu'à la main",
         identical(aq$observations, attendu) && identical(as.numeric(dq$cochees), setdiff(obs, obs[c(3, 9)])) &&
           memes_composantes(aq, ajuster(pq, definir_modele(pq, "PLS-DA", x = x, classe = u("variete"), A = 2, observations = attendu))))
do <- definir_modele(p, "OPLS-DA", x = x, classe = u("variete"), A = 0, condition = 'variete != "Reinette"')
verifier("OPLS-DA : trois classes restent, deux composantes prédictives", identical(do$Ap, 2))
dc <- definir_modele(p, "PCA", x = x, A = 2, condition = "sucre > 11.5")
classes <- modeles_classe(p, dc, u("culture"))
verifier("modèles de classe : chaque classe, ses observations cochées et celles où la condition est vraie",
         length(classes) == 2 && all(vapply(classes, function(e) {
           k <- match(e$groupe_classes$classe, c("bio", "conventionnel"))
           identical(e$observations, garde(df$culture == c("bio", "conventionnel")[k] & df$sucre > 11.5)) &&
             identical(as.numeric(e$cochees), garde(df$culture == c("bio", "conventionnel")[k])) &&
             identical(resoudre_observations(p, e)$observations, e$observations)
         }, TRUE)))
# les régressions, contre lm(), nlme::lme(), glm() et mgcv::gam() sur les lignes de la condition
dl <- definir_modele(p, "LM", x = u("sucre", "fermete", "polyphenols"), y = u("note_gout"), echelle = "none", condition = 'verger != "B"')
ml <- ajuster(p, dl)
sous <- subset(df, verger != "B")
l <- lm(note_gout ~ sucre + fermete + polyphenols, data = sous)
verifier(sprintf("LM : coefficients, AIC et R² = lm() sur subset(verger != \"B\") (%d observations, %d sans polyphénols laissées de côté)", ml$N, length(ml$exclues)),
         proche(ml$resultat$coefficients$estimation, coef(l)) && proche(ml$resultat$AIC, AIC(l)) && proche(ml$resultat$r2$r2, summary(l)$r.squared) &&
           ml$N == nobs(l) && length(ml$exclues) == sum(is.na(sous$polyphenols)))
trc <- subset(tr, site != "S2" & annee >= 2003)
dm <- definir_modele(pr, "LMM", x = ur("x1", "x2"), y = ur("hauteur"), echelle = "none", aleatoires = list(list(by = "id", id = "site")),
                     condition = 'site != "S2" & annee >= 2003')
mm <- ajuster(pr, dm)
lme_ml <- nlme::lme(hauteur ~ x1 + x2, random = ~ 1 | site, data = trc, method = "ML", na.action = stats::na.omit,
                    control = nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE))
lme_reml <- nlme::lme(hauteur ~ x1 + x2, random = ~ 1 | site, data = trc, method = "REML", na.action = stats::na.omit,
                      control = nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE))
verifier(sprintf("LMM, un identifiant dans la condition : coefficients (REML) et AIC (ML) = nlme::lme() sur ces lignes (%d observations)", mm$N),
         proche(mm$resultat$coefficients$estimation, nlme::fixef(lme_reml), 1e-6) && proche(mm$resultat$AIC, AIC(lme_ml), 1e-6) && mm$N == nrow(stats::na.omit(trc[c("hauteur", "x1", "x2")])))
dg <- definir_modele(pr, "GLM", x = ur("x1"), y = ur("compte"), famille = "poisson", echelle = "none", condition = "x2 < 0.7 & secteur == \"a\"")
mg <- ajuster(pr, dg)
g <- glm(compte ~ x1, family = poisson, data = subset(tr, x2 < 0.7 & secteur == "a"))
verifier("GLM de Poisson : coefficients et AIC = glm() sur ces lignes", proche(mg$resultat$coefficients$estimation, coef(g), 1e-6) && proche(mg$resultat$AIC, AIC(g), 1e-6))
da <- definir_modele(pr, "GAM", x = ur("x1", "x2"), y = ur("hauteur"), famille = "gaussienne", lisses = ur("x2"), echelle = "none", condition = "annee != 2005")
ma <- ajuster(pr, da)
ga <- mgcv::gam(hauteur ~ x1 + s(x2, k = 10), data = subset(tr, annee != 2005), method = "REML")
verifier("GAM : coefficients et edf de la courbe = mgcv::gam() sur ces lignes",
         proche(ma$resultat$coefficients$estimation, unname(coef(ga)[1:2]), 1e-6) && proche(ma$resultat$courbes$edf, summary(ga)$s.table[1, 1], 1e-6))
# les autres analyses : les mêmes résultats que leurs observations cochées à la main
for (k in list(list(type = "TREE", y = u("note_gout"), compare = function(a, b) identical(a$groupes, b$groupes) && proche(a$complet$cptable, b$complet$cptable)),
               list(type = "LDA", classe = u("variete"), compare = function(a, b) proche(a$proba, b$proba) && proche(a$vc$proba, b$vc$proba)),
               list(type = "KMEANS", options = list(k = 3), compare = function(a, b) identical(a$groupe, b$groupe) && proche(a$centres, b$centres)))) {
  dt <- definir_modele(p, k$type, x = x, y = k$y, classe = k$classe, echelle = if (k$type == "KMEANS") "UV" else "none", options = k$options, condition = cond)
  a <- ajuster(p, dt)
  b <- ajuster(p, definir_modele(p, k$type, x = x, y = k$y, classe = k$classe, echelle = if (k$type == "KMEANS") "UV" else "none", options = k$options, observations = choisies))
  verifier(sprintf("%s : ses %d observations, les mêmes résultats qu'à la main", k$type, a$N), identical(a$observations, choisies) && k$compare(a, b))
}
set.seed(42)
ab <- matrix(stats::rpois(40 * 6, 4), 40, 6, dimnames = list(NULL, sprintf("sp%d", 1:6)))
env <- data.frame(nom = sprintf("r%02d", 1:40), temp = stats::rnorm(40, 12), ph = stats::rnorm(40, 6.5, 0.4), habitat = rep(c("foret", "marais", "prairie", "foret"), 10),
                  ab, stringsAsFactors = FALSE)
po <- projet_depuis_tableau(env, "relevés")
uo <- function(...) numeros(po, c(...))
choisis_o <- numeros_observations(po)[env$habitat != "marais" & env$temp > 11]
for (type in c("RDA", "CA")) {
  dr <- definir_modele(po, type, x = if (type == "RDA") uo("temp", "ph") else uo(sprintf("sp%d", 1:6)), y = if (type == "RDA") uo(sprintf("sp%d", 1:6)),
                       echelle = "none", options = if (type == "RDA") list(transformation = "hellinger", permutations = 99, graine = 1), condition = 'habitat != "marais" & temp > 11')
  a <- ajuster(po, dr)
  b <- ajuster(po, definir_modele(po, type, x = if (type == "RDA") uo("temp", "ph") else uo(sprintf("sp%d", 1:6)), y = if (type == "RDA") uo(sprintf("sp%d", 1:6)),
                                  echelle = "none", options = if (type == "RDA") list(transformation = "hellinger", permutations = 99, graine = 1), observations = choisis_o))
  verifier(sprintf("%s : ses %d observations, les mêmes valeurs propres%s qu'à la main", type, a$N, if (type == "RDA") ", R² et test" else ""),
           identical(a$observations, choisis_o) && proche(if (type == "RDA") a$noyau$contraint$eig else a$noyau$eig, if (type == "RDA") b$noyau$contraint$eig else b$noyau$eig) &&
             (type == "CA" || (proche(a$r2, b$r2) && identical(a$test$p, b$test$p))))
}

cat("Refus\n")
intrus <- file.path(tempdir(), "intrus.txt")
for (k in list(list("sucer > 12", "Variable introuvable : sucer"), list("site == \"A\"", "Variable introuvable : site"),
               list(sprintf("file.create(%s)", deparse(intrus)), "file.create() » n'est pas permis"), list("system(\"echo non\") == 0", "system() » n'est pas permis"),
               list("get(\"sucre\") > 12", "get() » n'est pas permis"), list("eval(parse(text = \"sucre\")) > 12", "eval() » n'est pas permis"),
               list("df$sucre > 12", "« $ » n'est pas permis"), list("sucre[1:3] > 12", "« [ » n'est pas permis"), list("(sucre <- 1) > 0", "« <- » n'est pas permis"),
               list("base::nchar(verger) == 1", "base::nchar » n'est pas permis"), list("(function(z) z)(sucre) > 12", "n'est pas permis"),
               list("{ sucre > 12 }", "« { » n'est pas permis"), list("sucre > 12; TRUE", "une seule expression"), list("sucre >", "Condition illisible"),
               list("c(TRUE, FALSE)", "une valeur par observation (120) ; elle en donne 2"), list("mean(sucre) > 12", "elle en donne 1"),
               list("variete", "elle donne du texte"), list("interaction(variete, verger)", "elle donne du texte"), list("sucre", "d'autres nombres (13"),
               list("quantile(sucre, c(1, , 2))", "il y manque un argument"))) {
  m <- refus(valeurs_condition(p, k[[1]]))
  verifier(sprintf("%s : refusé (« %s »)", k[[1]], substr(m, 1, 60)), grepl(k[[2]], m, fixed = TRUE))
}
verifier("le code refusé n'a pas été exécuté", !file.exists(intrus))
verifier("aucune observation cochée ne la remplit : le modèle est refusé, et le dit",
         grepl("Aucune des 120 observations cochées ne remplit la condition sucre > 100", refus(definir_modele(p, "PCA", x = x, A = 2, condition = "sucre > 100")), fixed = TRUE))
verifier("en anglais aussi", {
  ancien <- options(nuee.langue = "en")
  on.exit(options(ancien), add = TRUE)
  m1 <- refus(valeurs_condition(p, "system(\"echo non\") == 0"))
  m2 <- refus(valeurs_condition(p, "sucer > 12"))
  options(ancien)
  grepl("isn't allowed in a condition", m1) && grepl("Variable not found: sucer", m2)
})
# un projet reçu : sa définition porte une condition qui exécuterait du code ; refusée à l'ajustement
fichier <- tempfile(fileext = ".nuee")
recu <- p
dr <- definir_modele(p, "PCA", x = x, A = 2, nom = "M9")
dr$condition <- sprintf("file.create(%s)", deparse(intrus))
dr$cochees <- dr$observations
dr$app <- TRUE
recu$models <- c(recu$models, list(dr))
enregistrer_projet(recu, fichier)
relu <- lire_projet(fichier)
verifier("la condition d'un projet reçu qui appelle une fonction non permise : refusée à l'ajustement, sans rien exécuter",
         grepl("n'est pas permis", refus(ajuster(relu, Find(function(x) identical(x$name, "M9"), relu$models)))) && !file.exists(intrus))

cat("Cases cochées, réajustements\n")
cochees <- obs[-(1:10)]
d <- definir_modele(p, "PCA", x = x, A = 2, observations = cochees, condition = "sucre > 12")
r <- reglages_modele(p, d)
verifier("les cases cochées restent : les décochées seules dans les réglages (Modifier le modèle), et la condition",
         identical(as.numeric(r$exclues), obs[1:10]) && identical(r$condition, "sucre > 12") && identical(d$observations, cochees[df$sucre[-(1:10)] > 12]))
sans <- definir_modele(p, "PCA", x = x, A = 2, observations = setdiff(obs, r$exclues), condition = "")
verifier("la condition retirée : les observations qu'elle écartait reviennent", identical(sans$observations, cochees) && is.null(sans$condition) && is.null(sans$cochees))
pm <- p
k <- which(df$sucre <= 12 & seq_along(obs) > 10)[1]  # une pomme cochée sous 12...
pm$donnees[as.character(obs[k]), as.character(u("sucre"))] <- 15  # ...passe au-dessus
verifier(sprintf("réajusté après un changement des données : la condition refaite (%d puis %d observations)", length(d$observations), ajuster(pm, d)$N),
         ajuster(pm, d)$N == length(d$observations) + 1 && identical(resoudre_observations(pm, d)$observations, sort(c(d$observations, obs[k]))))
verifier("l'ajustement automatique sur les observations de la condition", autoajuster(p, d) == autoajuster(p, definir_modele(p, "PCA", x = x, A = 2, observations = d$observations)))
mc <- ajuster(pr, dm)
dsel <- definition_selection(dm, mc, "x1")
verifier("Ajuster ce modèle (sélection par l'AIC) garde la condition et les cases", identical(dsel$condition, dm$condition) && identical(dsel$cochees, dm$cochees) &&
           identical(ajuster(pr, dsel)$observations, mc$observations))
dcar <- definition_car1(pr, dm, list(by = "variable", variable = ur("annee")), list(list(by = "effets"), list(by = "variable", variable = ur("secteur"))))
verifier("Ajuster avec CAR1 garde la condition", identical(dcar$condition, dm$condition) && identical(ajuster(pr, dcar)$N, mc$N))
verifier("sans condition, la définition ne change pas (un projet d'avant)", identical(resoudre_observations(p, p$models[[1]]), p$models[[1]]) && is.null(p$models[[1]]$condition))

cat("Enregistrer et relire\n")
pe <- p
de <- definir_modele(p, "PLS", x = x, y = u("note_gout"), A = 2, nom = "M5", observations = cochees, condition = 'variete == "Gala" & sucre > 11')
de$app <- TRUE
pe$models <- c(pe$models, list(de))
fichier <- tempfile(fileext = ".nuee")
enregistrer_projet(pe, fichier)
relu <- lire_projet(fichier)
dr <- Find(function(x) identical(x$name, "M5"), relu$models)
verifier("la condition et les cases relues ; le même modèle ; les mêmes réglages",
         identical(dr$condition, de$condition) && identical(as.numeric(dr$cochees), as.numeric(de$cochees)) &&
           memes_composantes(ajuster(relu, dr), ajuster(pe, de)) && identical(reglages_modele(relu, dr)$exclues, reglages_modele(pe, de)$exclues))
verifier("la liste des observations enregistrée est celle de la condition : un Nuée d'avant ajuste le même modèle", identical(as.numeric(dr$observations), de$observations))

cat("Prédire, texte des méthodes\n")
m <- ajuster(p, d)
pred <- predire(p, m, d, obs)
verifier("les observations écartées par la condition sont prédites, hors du modèle", !anyNA(pred$t1) && sum(obs %in% m$observations) == m$N && m$N < length(obs))
textes <- list(PCA = list(p, d), LM = list(p, dl), TREE = list(p, definir_modele(p, "TREE", x = x, y = u("note_gout"), echelle = "none", condition = cond)),
               LDA = list(p, definir_modele(p, "LDA", x = x, classe = u("variete"), echelle = "none", condition = cond)),
               KMEANS = list(p, definir_modele(p, "KMEANS", x = x, options = list(k = 3), condition = cond)),
               RDA = list(po, definir_modele(po, "RDA", x = uo("temp", "ph"), y = uo(sprintf("sp%d", 1:6)), echelle = "none", options = list(permutations = 0), condition = "temp > 11")),
               CA = list(po, definir_modele(po, "CA", x = uo(sprintf("sp%d", 1:6)), echelle = "none", condition = "temp > 11")))
for (type in names(textes)) {
  pt <- textes[[type]][[1]]
  dt <- textes[[type]][[2]]
  mt <- ajuster(pt, dt)
  fr <- texte_methodes(pt, dt, mt, "fr")$paragraphes[1]
  en <- texte_methodes(pt, dt, mt, "en")$paragraphes[1]
  verifier(sprintf("%s : « %s »", type, substr(en, 1, 90)),
           grepl(sprintf("%s %d observations où %s", if (type == "CA") "des" else "les", mt$N, dt$condition), fr, fixed = TRUE) &&
             grepl(sprintf("%s %d observations where %s", if (type == "CA") "of the" else "the", mt$N, dt$condition), en, fixed = TRUE))
}

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
