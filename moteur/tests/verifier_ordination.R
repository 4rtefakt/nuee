# Vérifie l'analyse de redondance et l'analyse des correspondances (moteur/R/ordination.R) :
# contre des calculs directs (lm.fit, svd, cor, chisq.test), contre MASS::corresp (livré avec R)
# et, quand il est installé, contre le paquet vegan (rda, cca, anova.cca, RsquareAdj,
# ordiR2step, varpart) : mêmes valeurs propres, scores, R², pseudo-F et, pour les mêmes
# permutations (passées à vegan), mêmes valeurs p. Puis ce que l'interface en reçoit, le texte
# des méthodes, les tableaux exportés, les refus.
#   Rscript moteur/tests/verifier_ordination.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-8) isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), tolerance = tol, check.attributes = FALSE))
# deux matrices de scores égales au signe de chaque axe près (un axe peut sortir retourné)
au_signe_pres <- function(a, b, tol = 1e-7) {
  a <- unname(as.matrix(a)); b <- unname(as.matrix(b))
  if (!identical(dim(a), dim(b))) return(FALSE)
  for (j in seq_len(ncol(a))) {
    s <- if (sum(a[, j] * b[, j]) < 0) -1 else 1
    if (!isTRUE(all.equal(a[, j], s * b[, j], tolerance = tol))) return(FALSE)
  }
  TRUE
}
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

# vegan, s'il est installé (NUEE_VEGAN : une bibliothèque où il l'est)
if (nzchar(Sys.getenv("NUEE_VEGAN"))) .libPaths(c(Sys.getenv("NUEE_VEGAN"), .libPaths()))
avec_vegan <- requireNamespace("vegan", quietly = TRUE)
cat(if (avec_vegan) sprintf("(vegan %s installé : comparaisons avec lui)\n", utils::packageVersion("vegan")) else "(vegan absent : calculs directs seulement)\n")

# ---------------------------------------------------------------- les données

# 40 relevés de 15 espèces (effectifs de Poisson le long de deux gradients et selon l'habitat),
# leur milieu (température, pH, altitude, pente, habitat), le site (8 sites de 5 relevés).
set.seed(42)
n <- 40
g1 <- stats::runif(n, -2, 2)
g2 <- stats::rnorm(n)
habitat <- factor(sample(c("prairie", "foret", "marais"), n, TRUE), levels = c("foret", "marais", "prairie"))
effet <- c(foret = -0.3, marais = 0.4, prairie = 0)
mu <- sapply(1:15, function(j) exp(0.8 + sin(j) * g1 + cos(j) * g2 * 0.7 + effet[as.character(habitat)] * ((j %% 3) - 1)))
abondances <- matrix(stats::rpois(n * 15, mu), n, 15, dimnames = list(NULL, sprintf("sp%02d", 1:15)))
abondances[rowSums(abondances) == 0, 1] <- 1
env <- data.frame(temp = g1 * 3 + 12 + stats::rnorm(n, 0, 0.5), ph = g2 * 0.4 + 6.5 + stats::rnorm(n, 0, 0.2),
                  altitude = 800 - 40 * g1 + stats::rnorm(n, 0, 30), pente = stats::runif(n, 0, 30), habitat = habitat,
                  site = rep(sprintf("S%d", 1:8), each = 5), stringsAsFactors = FALSE)
df <- data.frame(nom = sprintf("r%02d", 1:n), env, abondances, stringsAsFactors = FALSE)
df$ph[7] <- NA  # une valeur manquante : ce relevé sort des modèles qui prennent le pH
p <- projet_depuis_tableau(df, "essai")
u <- function(...) numeros(p, c(...))
especes <- colnames(abondances)
garde <- seq_len(n) != 7
hel <- sqrt(abondances / rowSums(abondances))
centre <- function(M) scale(M, center = TRUE, scale = FALSE)
B <- 199

cat("RDA : calculs directs\n")
d <- definir_modele(p, "RDA", x = u("temp", "ph", "habitat"), y = u(especes), echelle = "none",
                    options = list(transformation = "hellinger", permutations = B, graine = 3))
m <- ajuster(p, d)
Xmm <- stats::model.matrix(~ temp + ph + habitat, env[garde, ])
Yc <- centre(hel[garde, ])
ajuste <- stats::lm.fit(Xmm, Yc)$fitted.values
k <- m$noyau
verifier(sprintf("%d observations (le relevé sans pH laissé de côté)", m$N), m$N == 39 && identical(m$exclues, 7))
verifier("réponses : Hellinger, centrées, / racine(n - 1)", proche(m$donnees$Ybar, Yc / sqrt(38)))
verifier("valeurs propres contraintes = svd des valeurs ajustées par lm (variances)", proche(k$contraint$eig, (svd(ajuste)$d^2 / 38)[1:4]))
verifier("valeurs propres résiduelles = svd des résidus", proche(k$residuel$eig, (svd(Yc - ajuste)$d^2 / 38)[1:k$residuel$rang]))
R2 <- sum(ajuste^2) / sum(Yc^2)
q <- ncol(Xmm) - 1
verifier("R² et R² ajusté (Ezekiel)", proche(m$r2, R2) && proche(m$r2_ajuste, 1 - (1 - R2) * 38 / (39 - q - 1)))
F0 <- (R2 / q) / ((1 - R2) / (39 - q - 1))
P <- permutations_rda(m)
Fp <- apply(P, 1, function(ip) { a <- stats::lm.fit(Xmm, Yc[ip, ])$fitted.values; r2 <- sum(a^2) / sum(Yc^2); (r2 / q) / ((1 - r2) / (39 - q - 1)) })
verifier(sprintf("pseudo-F (%.3f) et p par permutation (%.3f) = boucle de lm sur les mêmes permutations", m$test$F, m$test$p),
         proche(m$test$F, F0) && identical(m$test$p, (sum(Fp >= F0 - sqrt(.Machine$double.eps)) + 1) / (B + 1)))
s2 <- scores_rda(m, 2)
s1 <- scores_rda(m, 1)
verifier("flèches (échelle 2) : corrélations des X quantitatifs aux scores lc", proche(fleches_rda(m, s2)[, 1:4], stats::cor(env[garde, c("temp", "ph")], s2$lc[, 1:4])))
verifier("flèches nulles sur les axes résiduels ; échelle 1 = échelle 2 × racine(λ / inertie)",
         all(fleches_rda(m, s2)[, -(1:4)] == 0) && proche(s1$biplot[, 1:4], sweep(s2$biplot[, 1:4], 2, sqrt(k$contraint$eig / k$totale), "*")))
verifier("centroïdes = moyennes des scores lc par habitat",
         proche(s2$centroides[, 1:4], apply(s2$lc[, 1:4], 2, function(x) tapply(x, env$habitat[garde], mean))))
verifier("échelle 2 : scores lc × scores des réponses, sur tous les axes, redonnent les réponses centrées", proche(s2$lc %*% t(s2$reponses), Yc, 1e-7))

cat("RDA partielle, permutations par site\n")
dz <- definir_modele(p, "RDA", x = u("temp", "habitat"), y = u(especes), echelle = "none",
                     options = list(transformation = "hellinger", permutations = B, graine = 5, z = u("pente"), strates = list(by = "variable", variable = u("site"))))
mz <- ajuster(p, dz)
Z <- centre(env$pente)
Yz <- stats::lm.fit(cbind(1, env$pente), centre(hel))$residuals
Xz <- stats::model.matrix(~ pente + temp + habitat, env)
az <- stats::lm.fit(Xz, centre(hel))$fitted.values - (centre(hel) - Yz)
kz <- mz$noyau
verifier("40 observations (sans le pH), inertie conditionnelle = part de la pente", mz$N == 40 && proche(kz$conditionnelle, sum((centre(hel) - Yz)^2) / 39))
verifier("valeurs propres contraintes de la RDA partielle", proche(kz$contraint$eig, (svd(az)$d^2 / 39)[1:kz$contraint$rang]))
R2c <- kz$conditionnelle / kz$totale
R2x <- kz$contraint$inertie / kz$totale
adj <- function(r, m) 1 - (1 - r) * 39 / (40 - m - 1)
verifier("R² ajusté partiel (Peres-Neto et al. 2006)", proche(mz$r2_ajuste, adj(R2x + R2c, 3 + 1) - adj(R2c, 1)))
Pz <- permutations_rda(mz)
verifier("permutations à l'intérieur des sites seulement", all(apply(Pz, 1, function(ip) all(env$site[ip] == env$site))) && any(Pz[1, ] != seq_len(n)))
verifier("mêmes permutations pour la même graine, d'autres pour une autre ; le hasard de la session intact", {
  set.seed(99); a <- stats::runif(1); set.seed(99)
  P1 <- permutations_lignes(40, 20, env$site, 5); b <- stats::runif(1)
  identical(P1, Pz[1:20, ]) && !identical(P1, permutations_lignes(40, 20, env$site, 6)) && identical(a, b)
})

cat("Analyse des correspondances : MASS::corresp et calculs directs\n")
dc <- definir_modele(p, "CA", x = u(especes), echelle = "none")
mc <- ajuster(p, dc)
kc <- mc$noyau
co <- MASS::corresp(abondances, nf = length(kc$eig))
verifier(sprintf("%d valeurs propres = carrés des corrélations canoniques de MASS::corresp", length(kc$eig)), proche(kc$eig, co$cor^2))
verifier("coordonnées standard des lignes et des colonnes = rscore et cscore (au signe près)", au_signe_pres(kc$u, co$rscore) && au_signe_pres(kc$v, co$cscore))
verifier("inertie totale = khi² / effectif total", proche(kc$totale, suppressWarnings(stats::chisq.test(abondances)$statistic) / sum(abondances)))
ctr <- mc$contributions
verifier("contributions : 1 par axe ; cos² : 1 par ligne sur tous les axes ; inerties : 1",
         proche(colSums(ctr$lignes$contribution), rep(1, length(kc$eig))) && proche(colSums(ctr$colonnes$contribution), rep(1, length(kc$eig))) &&
           proche(rowSums(ctr$lignes$cos2), rep(1, n)) && proche(sum(ctr$lignes$inertie), 1) && proche(sum(ctr$colonnes$inertie), 1))
sc1 <- scores_ca(mc, 1)
verifier("échelle 1 : distances entre lignes = distances du khi² entre profils", {
  prof <- abondances / rowSums(abondances)
  dchi <- as.matrix(stats::dist(sweep(prof, 2, sqrt(colSums(abondances) / sum(abondances)), "/")))
  proche(as.matrix(stats::dist(sc1$sites)), dchi)
})

cat("Sélection pas à pas et partition de la variance\n")
dsel <- definir_modele(p, "RDA", x = u("temp", "altitude", "pente", "habitat"), y = u(especes), echelle = "none",
                       options = list(transformation = "hellinger", permutations = B, graine = 7), blocs = c(stats::setNames(c("climat", "climat"), u("temp", "altitude")), stats::setNames("terrain", u("pente"))))
msel <- ajuster(p, dsel)
sel <- selection_rda(msel)
r2_seul <- function(v) {
  mm <- stats::model.matrix(stats::as.formula(paste("~", v)), env)
  a <- stats::lm.fit(mm, centre(hel))$fitted.values
  adj(sum(a^2) / sum(centre(hel)^2), ncol(mm) - 1)
}
verifier(sprintf("premier pas : le terme au plus grand R² ajusté (%s)", sel$etapes$terme[1]),
         sel$etapes$terme[1] == c("temp", "altitude", "pente", "habitat")[which.max(vapply(c("temp", "altitude", "pente", "habitat"), r2_seul, 0))] &&
           proche(sel$etapes$r2_ajuste[1], max(vapply(c("temp", "altitude", "pente", "habitat"), r2_seul, 0))))
verifier(sprintf("arrêt (%s) ; les termes retenus ont p ≤ 0,05", sel$arret), sel$arret %in% c("tous", "r2", "r2_tous", "p") && all(sel$etapes$p[sel$etapes$retenu] <= 0.05))
part <- partition_rda(msel)
f <- part$fractions
verifier("trois ensembles (climat, terrain, sans bloc) : fractions de somme 1, [a] + [d] + [f] + [g] = R² ajusté du climat",
         nrow(f) == 8 && proche(sum(f$r2_ajuste), 1) && proche(sum(f$r2_ajuste[c(1, 4, 6, 7)]), part$ensembles$r2_ajuste[1]))

if (avec_vegan) {
  cat("Comparaison avec vegan\n")
  # (les permutations de Nuée passées à vegan, avec une description que permute sait afficher)
  pv <- function(P) { attr(P, "control") <- permute::how(nperm = nrow(P)); P }
  ve <- env[garde, ]
  rv <- vegan::rda(hel[garde, ] ~ temp + ph + habitat, data = ve)
  verifier("valeurs propres et inerties = vegan::rda", proche(k$contraint$eig, rv$CCA$eig) && proche(k$residuel$eig, rv$CA$eig) &&
             proche(k$totale, rv$tot.chi) && proche(k$contraint$inertie, rv$CCA$tot.chi))
  ra <- vegan::RsquareAdj(rv)
  verifier("R² et R² ajusté = RsquareAdj", proche(m$r2, ra$r.squared) && proche(m$r2_ajuste, ra$adj.r.squared))
  axes <- seq_along(c(rv$CCA$eig, rv$CA$eig))
  for (e in 1:2) {
    sv <- vegan::scores(rv, choices = axes, display = c("sites", "lc", "species", "bp", "cn"), scaling = e)
    sn <- scores_rda(m, e)
    verifier(sprintf("échelle %d : scores wa, lc, réponses, flèches (temp, pH), centroïdes = vegan::scores", e),
             proche(sn$sites, sv$sites) && proche(sn$lc, sv$constraints) && proche(sn$reponses, sv$species) &&
               proche(sn$biplot[c("temp", "ph"), ], sv$biplot[c("temp", "ph"), ]) &&
               proche(sn$centroides, sv$centroids[paste0("habitat", levels(env$habitat)), ]))
  }
  av <- vegan::anova.cca(rv, permutations = pv(P))
  verifier("test global : F et p = anova.cca avec les mêmes permutations", proche(m$test$F, av$F[1]) && identical(m$test$p, av[["Pr(>F)"]][1]))
  ta <- tests_axes_rda(m, P)
  vax <- vegan::anova.cca(rv, by = "axis", permutations = pv(P))
  verifier("tests par axe = anova.cca(by = \"axis\")", proche(ta$F, vax$F) && isTRUE(all.equal(ta$p, vax[["Pr(>F)"]])) && proche(ta$ddl, vax$Df))
  tt <- tests_termes_rda(m, P)
  vma <- vegan::anova.cca(rv, by = "margin", permutations = pv(P))
  verifier("tests par terme (marginaux) = anova.cca(by = \"margin\")", proche(tt$F, vma$F) && isTRUE(all.equal(tt$p, vma[["Pr(>F)"]])) &&
             proche(tt$inertie, vma$Variance) && proche(tt$ddl, vma$Df))
  rzv <- vegan::rda(hel ~ temp + habitat + Condition(pente), data = env)
  verifier("RDA partielle : inerties, valeurs propres, R² ajusté = vegan", proche(kz$conditionnelle, rzv$pCCA$tot.chi) && proche(kz$contraint$eig, rzv$CCA$eig) &&
             proche(kz$residuel$eig, rzv$CA$eig) && proche(mz$r2_ajuste, vegan::RsquareAdj(rzv)$adj.r.squared))
  avz <- vegan::anova.cca(rzv, permutations = pv(Pz))
  verifier("RDA partielle permutée par site : F et p = anova.cca", proche(mz$test$F, avz$F[1]) && identical(mz$test$p, avz[["Pr(>F)"]][1]))
  verifier("RDA partielle : tests par axe et par terme = anova.cca", {
    a <- tests_axes_rda(mz, Pz); va <- vegan::anova.cca(rzv, by = "axis", permutations = pv(Pz))
    t <- tests_termes_rda(mz, Pz); vt <- vegan::anova.cca(rzv, by = "margin", permutations = pv(Pz))
    proche(a$F, va$F) && isTRUE(all.equal(a$p, va[["Pr(>F)"]])) && proche(t$F, vt$F) && isTRUE(all.equal(t$p, vt[["Pr(>F)"]]))
  })
  ss <- vegan::scores(rzv, choices = seq_along(c(rzv$CCA$eig, rzv$CA$eig)), display = c("sites", "lc", "species", "bp"), scaling = 2)
  sz <- scores_rda(mz, 2)
  verifier("RDA partielle : scores (échelle 2) = vegan", proche(sz$sites, ss$sites) && proche(sz$lc, ss$constraints) && proche(sz$reponses, ss$species) &&
             proche(sz$biplot["temp", ], ss$biplot["temp", ]))
  rsel <- vegan::rda(hel ~ temp + altitude + pente + habitat, data = env)
  r0 <- vegan::rda(hel ~ 1, data = env)
  suppressPackageStartupMessages(library("vegan", character.only = TRUE))  # (ordiR2step réévalue des appels à rda())
  pas <- suppressMessages(vegan::ordiR2step(r0, scope = stats::formula(rsel), permutations = pv(permutations_rda(msel)), trace = FALSE))
  detach("package:vegan")
  retenus_v <- attr(stats::terms(pas), "term.labels")
  verifier(sprintf("sélection pas à pas = ordiR2step (%s)", paste(sel$retenus, collapse = ", ")),
           identical(sel$retenus, retenus_v) && (!length(retenus_v) || proche(sel$etapes$p[sel$etapes$retenu], pas$anova[["Pr(>F)"]][seq_along(retenus_v)])) &&
             (!length(retenus_v) || proche(sel$etapes$r2_ajuste[sel$etapes$retenu], pas$anova$R2.adj[seq_along(retenus_v)])))
  vp3 <- vegan::varpart(hel, ~ temp + altitude, ~ pente, ~ habitat, data = env)
  verifier("partition en trois ensembles = varpart (fractions [a] à [h])", proche(f$r2_ajuste, vp3$part$indfract$Adj.R.square))
  verifier("partition : R² ajustés des ensembles et de leurs réunions = varpart", proche(part$ensembles$r2_ajuste, vp3$part$fract$Adj.R.square) &&
             proche(part$ensembles$ddl, vp3$part$fract$Df))
  dsel2 <- definir_modele(p, "RDA", x = u("temp", "altitude", "habitat"), y = u(especes), echelle = "none",
                          options = list(transformation = "hellinger", permutations = 0), blocs = stats::setNames(c("climat", "climat"), u("temp", "altitude")))
  part2 <- partition_rda(ajuster(p, dsel2))
  vp2 <- vegan::varpart(hel, ~ temp + altitude, ~ habitat, data = env)
  verifier("partition en deux ensembles = varpart", proche(part2$fractions$r2_ajuste, vp2$part$indfract$Adj.R.square))
  cv <- vegan::cca(abondances)
  verifier("CA : valeurs propres et inertie = vegan::cca", proche(kc$eig, cv$CA$eig) && proche(kc$totale, cv$tot.chi))
  verifier("CA : scores des lignes et des colonnes en échelles 1, 2 et 3 = vegan::scores", all(vapply(1:3, function(e) {
    sv <- vegan::scores(cv, choices = seq_along(kc$eig), display = c("sites", "species"), scaling = e)
    sn <- scores_ca(mc, e)
    proche(sn$sites, sv$sites) && proche(sn$reponses, sv$species)
  }, TRUE)))
  # sur des données réelles : les prairies dunaires (vegan::dune), corrélations, un facteur
  e <- new.env()
  utils::data("dune", "dune.env", package = "vegan", envir = e)
  pd <- projet_depuis_tableau(data.frame(nom = rownames(e$dune), A1 = e$dune.env$A1, Management = factor(as.character(e$dune.env$Management)), e$dune,
                                         check.names = FALSE, stringsAsFactors = FALSE), "dune")
  dd <- definir_modele(pd, "RDA", x = numeros(pd, c("A1", "Management")), y = numeros(pd, colnames(e$dune)), echelle = "none",
                       options = list(transformation = "aucune", reduire = TRUE, permutations = 99, graine = 1))
  md <- ajuster(pd, dd)
  rd <- vegan::rda(e$dune ~ A1 + Management, data = data.frame(A1 = e$dune.env$A1, Management = factor(as.character(e$dune.env$Management))), scale = TRUE)
  sd2 <- vegan::scores(rd, choices = 1:4, display = c("sites", "species", "bp", "cn"), scaling = 2)
  sn <- scores_rda(md, 2)
  verifier("vegan::dune, RDA sur les corrélations : valeurs propres, R² ajusté, scores, test = vegan",
           proche(md$noyau$contraint$eig, rd$CCA$eig) && proche(md$noyau$residuel$eig, rd$CA$eig) && proche(md$r2_ajuste, vegan::RsquareAdj(rd)$adj.r.squared) &&
             proche(sn$sites[, 1:4], sd2$sites) && proche(sn$reponses[, 1:4], sd2$species) && proche(sn$biplot["A1", 1:4], sd2$biplot["A1", ]) &&
             proche(sn$centroides[, 1:4], sd2$centroids[paste0("Management", sort(unique(as.character(e$dune.env$Management)))), ]) &&
             identical(md$test$p, vegan::anova.cca(rd, permutations = pv(permutations_rda(md)))[["Pr(>F)"]][1]))
  pc <- vegan::decostand(e$dune, "normalize")  # (la transformation en cordes)
  dco <- definir_modele(pd, "RDA", x = numeros(pd, c("A1", "Management")), y = numeros(pd, colnames(e$dune)), echelle = "none", options = list(transformation = "corde", permutations = 0))
  rco <- vegan::rda(pc ~ A1 + Management, data = data.frame(A1 = e$dune.env$A1, Management = factor(as.character(e$dune.env$Management))))
  verifier("transformation en cordes = decostand(\"normalize\")", proche(ajuster(pd, dco)$noyau$contraint$eig, rco$CCA$eig))
  # les lichens des landes à pins (vegan::varespec) et leur sol (varechem) : une sélection en plusieurs pas
  utils::data("varespec", "varechem", package = "vegan", envir = e)
  pv2 <- projet_depuis_tableau(data.frame(nom = rownames(e$varespec), e$varechem, e$varespec, check.names = FALSE), "varespec")
  dv <- definir_modele(pv2, "RDA", x = numeros(pv2, names(e$varechem)), y = numeros(pv2, names(e$varespec)), echelle = "none",
                       options = list(transformation = "aucune", permutations = 199, graine = 11))
  mv <- ajuster(pv2, dv)
  sv <- selection_rda(mv)
  hv <- e$varespec
  vc <- e$varechem
  suppressPackageStartupMessages(library("vegan", character.only = TRUE))
  pasv <- suppressMessages(vegan::ordiR2step(vegan::rda(hv ~ 1, data = vc), scope = stats::formula(vegan::rda(hv ~ ., data = vc)),
                                             permutations = pv(permutations_rda(mv)), trace = FALSE))
  detach("package:vegan")
  tv <- attr(stats::terms(pasv), "term.labels")
  verifier(sprintf("vegan::varespec : sélection pas à pas en %d pas (%s), R² ajustés et p = ordiR2step", length(tv), paste(tv, collapse = ", ")),
           length(tv) >= 2 && identical(sv$retenus, tv) && proche(sv$etapes$r2_ajuste[sv$etapes$retenu], pasv$anova$R2.adj[seq_along(tv)]) &&
             proche(sv$etapes$p[sv$etapes$retenu], pasv$anova[["Pr(>F)"]][seq_along(tv)]))
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
resultat <- function(id) { force(id); fonction_analyse("resultat_", etat$modeles[[id]]$definition$type)(id) }  # (serveur.R : resultat_modele)
r <- resultat(garder("M1", d, m))
verifier("RDA : analyse sans prédiction, chiffres, vues, scores en échelles 1 et 2 pour 10 axes au plus",
         isTRUE(r$analyse) && identical(r$prediction, FALSE) && grepl("^R2aj ", r$chiffres) && identical(as.character(r$vues_defaut), c("ordination", "valeurs_propres")) &&
           nrow(r$observations) == 39 && identical(names(r$observations)[1:3], c("uid", "nom", "RDA1")) && ncol(r$ordination$`1`$sites) == 10 &&
           identical(as.character(r$ordination$`2`$noms_biplot), c("temp", "ph")) && nrow(r$ordination$`2`$centroides) == 3 && inherits(json(r), "json"))
verifier("RDA : ses réglages pour Modifier le modèle (options, Z)", identical(r$reglages$options$transformation, "hellinger") &&
           identical(as.numeric(resultat(garder("M2", dz, mz))$reglages$options$z), u("pente")))
rt <- demande_analyse_rda_tests(list(modele = "M1"))
verifier("tests à la demande : par axe (4 + résidus) et par terme (3 + résidus), gardés avec le modèle",
         nrow(rt$axes) == 5 && nrow(rt$termes) == 4 && !is.null(etat$modeles$M1$modele$tests) && inherits(json(rt), "json"))
rs <- demande_analyse_rda_selection(list(modele = "M1"))
verifier("sélection à la demande", length(rs$etapes$terme) >= 1 && inherits(json(rs), "json"))
verifier("partition refusée sans deux ensembles, et pour une RDA partielle",
         grepl("deux ou trois", refus(demande_analyse_rda_partition(list(modele = "M1")))) && grepl("sans covariables", refus(demande_analyse_rda_partition(list(modele = "M2")))))
invisible(garder("M3", dsel, msel))
rp <- demande_analyse_rda_partition(list(modele = "M3"))
verifier("partition à la demande", nrow(rp$fractions) == 8 && inherits(json(rp), "json"))
rc <- resultat(garder("M4", dc, mc))
verifier("CA : contributions et cos² des lignes et des colonnes, échelles 1 à 3", isTRUE(rc$analyse) && nrow(rc$contributions$lignes) == 40 &&
           all(c("ctr_CA1", "cos2_CA1", "masse", "inertie") %in% names(rc$contributions$colonnes)) && length(rc$ordination) == 3 && inherits(json(rc), "json"))
for (langue in c("fr", "en")) {
  tm <- texte_methodes(p, d, etat$modeles$M1$modele, langue, "essai")
  tc <- texte_methodes(p, dc, mc, langue, "essai")
  tp <- texte_methodes(p, dsel, etat$modeles$M3$modele, langue, "essai")
  verifier(sprintf("texte des méthodes (%s) : RDA, tests, sélection, partition ; CA ; références", langue),
           length(tm$paragraphes) >= 4 && any(grepl("Hellinger", tm$paragraphes)) && any(grepl("Blanchet", tm$paragraphes)) &&
             any(grepl("Legendre, P., & Gallagher", tm$references)) && any(grepl("Peres-Neto", tp$paragraphes[3])) &&
             any(grepl(if (langue == "fr") "correspondances" else "correspondence", tc$paragraphes)) && any(grepl("Greenacre", tc$references)))
}
tr <- tables_resultats(p, etat$modeles$M1$modele)
tc <- tables_resultats(p, mc)
verifier("tableaux exportés : des data.frame, observations d'abord ; tests et sélection quand ils sont calculés",
         all(vapply(c(tr, tc), is.data.frame, TRUE)) && names(tr)[1] == "observations" && all(c("tests_axes", "tests_termes", "selection") %in% names(tr)) &&
           nrow(tr$observations) == 39 && all(c("contributions_lignes", "cos2_colonnes") %in% names(tc)))
neg <- p
neg$donnees[1, as.character(u("temp"))] <- -1
verifier("refus : RDA sans Y, Y qualitative, Hellinger de valeurs négatives, CA négative ou d'une seule variable, permutations par observation",
         nzchar(refus(ajuster(p, definir_modele(p, "RDA", x = u("temp"), y = NULL, echelle = "none")))) &&
           grepl("habitat", refus(ajuster(p, definir_modele(p, "RDA", x = u("temp"), y = u("habitat"), echelle = "none")))) &&
           grepl("positives", refus(ajuster(neg, definir_modele(neg, "RDA", x = u("ph"), y = u("temp", "pente"), echelle = "none", options = list(transformation = "hellinger"))))) &&
           grepl("positives", refus(ajuster(neg, definir_modele(neg, "CA", x = u("temp", "pente"), echelle = "none")))) &&
           grepl("au moins deux", refus(ajuster(p, definir_modele(p, "CA", x = u("temp"), echelle = "none")))) &&
           grepl("plusieurs", refus(ajuster(p, definir_modele(p, "RDA", x = u("temp"), y = u(especes), echelle = "none",
                                                              options = list(strates = list(by = "variable", variable = u("altitude"))))))))
verifier("refus : un jeu de test, qu'une RDA ou une CA ne prédit pas",
         grepl("jeu de test", refus(definir_modele(p, "RDA", x = u("temp"), y = u(especes), echelle = "none", test = 1:5))) &&
           grepl("jeu de test", refus(definir_modele(p, "CA", x = u(especes), echelle = "none", test = 1:5))))
verifier("refus : trop de variables X pour les observations", grepl("degré de liberté", refus(ajuster(p, definir_modele(p, "RDA", x = u(especes[1:12]), y = u("temp", "ph", "altitude"),
                                                                                                      echelle = "none", observations = 1:12)))))

cat("Export vers R : script autonome et rapports\n")
dossier <- tempfile("ordination")
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
cas <- list(list(id = "M1", langue = "fr", quoi = "RDA, tests par axe et par terme, sélection"), list(id = "M2", langue = "en", quoi = "RDA partielle permutée par site"),
            list(id = "M3", langue = "fr", quoi = "RDA, partition de la variance"), list(id = "M4", langue = "en", quoi = "analyse des correspondances"))
for (k in cas) {
  e <- etat$modeles[[k$id]]
  r <- ecrire_script(p, e$definition, file.path(dossier, paste0(k$id, ".R")), "moteur", m = e$modele, version = "essai", langue = k$langue)
  n <- function(x) nom_ordination(x, k$langue)
  s <- lancer(r$script, c(n("m"), n("valeurs_propres"), if (e$modele$type == "RDA") c(n("r2"), n("test"))))
  texte <- readLines(r$script, encoding = "UTF-8")
  sm <- s[[n("m")]]
  memes <- length(s) && if (e$modele$type == "RDA") {
    proche(c(sm[[n("contraint")]][[n("valeurs_propres")]], sm[[n("residuel")]][[n("valeurs_propres")]]), c(e$modele$noyau$contraint$eig, e$modele$noyau$residuel$eig)) &&
      proche(s[[n("r2")]][[n("ajuste")]], e$modele$r2_ajuste) && identical(unname(s[[n("test")]][["p"]]), e$modele$test$p)
  } else proche(sm[[n("valeurs_propres")]], e$modele$noyau$eig)
  verifier(sprintf("%s (%s) : sans écart vu par le moteur ; lancé seul, mêmes valeurs propres%s ; R seul, en %s", k$id, k$quoi,
                   if (e$modele$type == "RDA") ", R² ajusté et p" else "", k$langue),
           is.null(r$ecart) && memes && !any(grepl("library(", sub("#.*$", "", texte), fixed = TRUE)) && !any(grepl("vegan::", sub("#.*$", "", texte), fixed = TRUE)) &&
             (k$langue == "fr" || (any(grepl("^redundancy_analysis <- function|^correspondence_analysis <- function", texte)) && !any(grepl("^ *#(fr|en)( |$)", texte)))))
  if (!is.null(r$ecart) || !isTRUE(memes)) cat("    écart :", r$ecart, "/", utils::tail(attr(s, "messages"), 3), "\n")
}
texte <- readLines(file.path(dossier, "M1.R"), encoding = "UTF-8")
verifier("le script en français : la dernière ligne des tests par axe et par terme est « Résidus »",
         sum(grepl("permutations, \"Résidus\")", texte, fixed = TRUE)) == 2 && !any(grepl("\"Residual\"", texte, fixed = TRUE)))
for (ext in c("Rmd", "qmd")) {
  r <- ecrire_script(p, d, file.path(dossier, paste0("rapport.", ext)), "moteur", m = etat$modeles$M1$modele, version = "essai", langue = if (ext == "Rmd") "fr" else "en",
                     donnees = "csv", rdata = TRUE)
  texte <- readLines(r$script, encoding = "UTF-8")
  if (!is.null(r$ecart)) cat("    écart :", r$ecart, "\n")
  verifier(sprintf("rapport %s : méthodes, code, résultats, références ; données en CSV et .RData à côté", ext),
           is.null(r$ecart) && any(grepl("^```\\{r modele\\}", texte)) && any(grepl("^## (Méthodes|Methods)", texte)) && any(grepl("^- .*Legendre", texte)) &&
             length(r$chemins) == 3 && all(file.exists(r$chemins)))
}

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
