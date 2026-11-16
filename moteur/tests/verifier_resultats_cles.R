# Vérifie les résultats clés des modèles et leur comparaison (moteur/R/resultats_cles.R), pour chaque
# type de modèle : le résumé existe et ses entrées sont bien formées, ses clés sont les références des
# documents (celles de app/interface/references.js) et ses valeurs celles du modèle ; puis le même
# modèle réajusté sur des données changées (une autre version du tableau) donne les drapeaux attendus
# (signe, seuil, hors de l'ancien intervalle, nouvelles et disparues, catégorie) et des phrases dans
# les deux langues, avec la virgule décimale en français. Enfin la comparaison elle-même : un résumé
# passé par JSON se compare comme l'original, une composante d'ACP retournée se remet dans le sens de
# l'autre, les valeurs par variable nombreuses à changer se résument, l'importance ordonne les phrases.
#   Rscript moteur/tests/verifier_resultats_cles.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}

# ---------------------------------------------------------------- outils

resume <- function(p, d, m = ajuster(p, d)) resume_resultats(p, list(definition = d, modele = m))
cles <- function(E) vapply(E, `[[`, "", "cle")
entree <- function(E, cle) Find(function(e) identical(e$cle, cle), E)
valeur <- function(E, cle) entree(E, cle)$valeur %||% NA_real_
comparee <- function(C, cle) Find(function(x) identical(x$cle, cle), C$entrees)
drapeau <- function(C, cle, f) isTRUE(comparee(C, cle)[[f]])
phrase <- function(C, cle) comparee(C, cle)$phrase %||% ""
contient <- function(texte, motif) isTRUE(grepl(motif, texte, fixed = TRUE))
proche <- function(a, b, tol = 1e-10) length(a) == length(b) && isTRUE(all(abs(as.numeric(a) - as.numeric(b)) <= tol * pmax(1, abs(as.numeric(b)))))
# (ce que l'interface reçoit puis renvoie : le JSON du serveur, relu comme serveur.R relit une demande)
par_json <- function(E) jsonlite::fromJSON(as.character(jsonlite::toJSON(E, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA)), simplifyVector = TRUE)
GROUPES <- c("ajustement", "estimations", "tests", "aleatoires", "composantes", "variables", "especes", "moyennes", "puissance", "jeu_test", "stocke")

# Les entrées d'un résumé bien formées : une clé unique et un libellé, un groupe connu, une valeur
# finie ou une catégorie, un intervalle de deux bornes dans l'ordre, une valeur p entre 0 et 1.
bien_forme <- function(E) {
  length(E) > 0 && !anyDuplicated(cles(E)) && all(vapply(E, function(e) {
    nzchar(e$cle) && nzchar(e$libelle) && e$groupe %in% GROUPES && (is.null(e$valeur) || is.finite(e$valeur)) && (!is.null(e$valeur) || !is.null(e$classe)) &&
      (is.null(e$ic) || (length(e$ic) == 2 && e$ic[1] <= e$ic[2])) && (is.null(e$p) || (e$p >= 0 && e$p <= 1))
  }, TRUE))
}
# Les références qu'un document peut citer, toutes trouvées (une clé, ou l'alias d'une entrée).
trouvees <- function(E, refs) {
  connues <- c(cles(E), unlist(lapply(E, function(e) as.character(e$alias))))
  manquantes <- setdiff(refs, connues)
  if (length(manquantes)) cat("    (références sans entrée : ", paste(manquantes, collapse = ", "), ")\n", sep = "")
  !length(manquantes)
}
# Les références qu'offre un modèle de composantes ou une régression, comme references.js les
# écrit (VALEURS_COMPOSANTES, VALEURS_REGRESSION, les coefficients et les VIP), d'après le modèle.
references_composantes <- function(m) {
  s <- m$statistiques
  cp <- s$composantes
  pls <- identical(m$type, "PLS")
  ny <- m$y$noms
  A <- nrow(cp)
  c("R2X", if (pls) "R2Y", "Q2", if (length(m$q2_vide)) "Q2_vide", if (length(m$gain)) "gain", if (!is.null(m$hierarchique$q2cum)) "Q2_honnete",
    if (pls) c("RMSEE", "RMSEcv", paste0("RMSEE.", ny), paste0("RMSEcv.", ny)), if (!is.null(s$classification)) "bien_classes", "A", "N", "K", "G",
    paste0("R2X.", seq_len(A)), if (pls) paste0("R2Y.", seq_len(A)), paste0("Q2.", seq_len(A)),
    if (length(s$cv_anova)) c("cvanova.p", "cvanova.F", paste0("cvanova.p.", ny), paste0("cvanova.F.", ny)),
    if (pls) paste0("vip.", s$variables$terme), if (pls) paste0("coef.", s$coefficients$terme[is.finite(s$coefficients[[paste0("CoeffCS ", ny[1])]])]))
}
# Une autre analyse : ce que l'interface en reçoit (son resultat_<type>, comme serveur.R le demande),
# que references.js lit comme un modèle de composantes : A, N, K, et G, RMSEE, RMSEcv et bien_classes
# quand elle les a (des remplaçants de projet() et modele() de serveur.R, comme verifier_forets.R).
etat <- new.env()
projet <- function(id) etat$projets[[id]]
modele <- function(id, a_jour = FALSE) etat$modeles[[id]]
interface_autre <- function(p, d, m) {
  q <- p
  q$id <- "P1"
  etat$projets <- list(P1 = q)
  etat$modeles <- list(M1 = list(projet = "P1", definition = d, modele = m))
  fonction_analyse("resultat_", d$type)("M1")
}
references_autre <- function(r) {
  y <- as.character(unlist(r$noms_y))[1]
  du_premier <- function(x) if (!is.null(x) && !is.na(y) && !is.null(x[[y]])) nombre_ou_rien(x[[y]])
  c("A", "N", "K", if (!is.null(r$G)) "G", if (!is.null(du_premier(r$RMSEE))) c("RMSEE", paste0("RMSEE.", y)),
    if (!is.null(du_premier(r$RMSEcv))) c("RMSEcv", paste0("RMSEcv.", y)), if (!is.null(r$classification)) "bien_classes")
}
# (et les mêmes valeurs que l'interface)
memes_valeurs_autre <- function(E, r) {
  y <- as.character(unlist(r$noms_y))[1]
  proche(valeur(E, "A"), r$A) && proche(valeur(E, "N"), r$N) && proche(valeur(E, "K"), r$K) && (is.null(r$G) || proche(valeur(E, "G"), r$G)) &&
    (is.null(r$classification) || proche(valeur(E, "bien_classes"), r$classification$vc$global)) &&
    (is.null(r$RMSEcv) || proche(valeur(E, "RMSEcv"), r$RMSEcv[[y]])) && (is.null(r$RMSEE) || proche(valeur(E, "RMSEE"), r$RMSEE[[y]]))
}
references_regression <- function(m) {
  r2 <- m$resultat$r2
  termes <- m$resultat$coefficients$terme
  c("N", "K", intersect(c("R2", "R2_ajuste", "R2_marginal", "R2_conditionnel", "deviance_expliquee"), c(if (!is.null(r2[["r2"]])) "R2", if (!is.null(r2[["r2_ajuste"]])) "R2_ajuste",
    if (!is.null(r2[["marginal"]])) "R2_marginal", if (!is.null(r2[["conditionnel"]])) "R2_conditionnel", if (!is.null(r2[["deviance_expliquee"]])) "deviance_expliquee")),
    "Q2", "RMSEcv", "AIC", "AICc", "BIC", paste0(rep(c("coef.", "est.", "ic.", "p."), each = length(termes)), termes))
}

# ---------------------------------------------------------------- régressions

cat("Régressions : les références des documents, puis un réajustement sur d'autres données\n")
# deux versions d'un tableau : l'effet de x1 change de signe, celui de x2 disparaît, la catégorie b
# n'a plus d'observation et la catégorie c en a désormais (la référence d reste la dernière)
tableau_lm <- function(graine, b1, b2, cats, n = 60) {
  set.seed(graine)
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  habitat <- rep(cats, length.out = n)
  site <- rep(sprintf("S%d", 1:6), length.out = n)
  annee <- rep(2001:2010, each = 6)[seq_len(n)]
  y <- 2 + b1 * x1 + b2 * x2 + c(a = 0, b = 0.5, c = -0.7, d = 0)[habitat] + rep(stats::rnorm(6, 0, 0.6), length.out = n) + stats::rnorm(n, 0, 0.5)
  comptes <- stats::rpois(n, exp(1 + 0.4 * b1 * x1 + 0.3 * b2 * x2))
  data.frame(nom = sprintf("o%02d", seq_len(n)), site = site, annee = annee, x1 = x1, x2 = x2, habitat = habitat, y = y, comptes = comptes, stringsAsFactors = FALSE)
}
pA <- projet_depuis_tableau(tableau_lm(1, 0.8, -0.5, c("a", "b", "d")), "avant")
pB <- projet_depuis_tableau(tableau_lm(2, -0.8, 0, c("a", "c", "d")), "après")
lm_de <- function(p) definir_modele(p, "LM", x = numeros(p, c("x1", "x2", "habitat")), y = numeros(p, "y"), observations = numeros_observations(p), echelle = c(defaut = "none"))
dA <- lm_de(pA)
mA <- ajuster(pA, dA)
A <- resume(pA, dA, mA)
co <- mA$resultat$coefficients
verifier("LM : un résumé bien formé, chaque référence d'un document trouvée (coef, est, ic et p de chaque terme)", bien_forme(A) && trouvees(A, references_regression(mA)))
verifier("LM : les valeurs du modèle (R², Q², AIC ; coefficient de x1, son intervalle et sa valeur p)",
         proche(valeur(A, "R2"), mA$resultat$r2$r2) && proche(valeur(A, "Q2"), mA$vc$Q2) && proche(valeur(A, "AIC"), mA$resultat$AIC) &&
           proche(valeur(A, "coef.x1"), co$estimation[co$terme == "x1"]) && proche(entree(A, "coef.x1")$ic, c(co$bas[co$terme == "x1"], co$haut[co$terme == "x1"])) &&
           proche(entree(A, "coef.x1")$p, co$p[co$terme == "x1"]) && identical(entree(A, "coef.x1")$groupe, "estimations"))
B <- resume(pB, lm_de(pB))
C <- comparer_resultats(A, B, "fr")
verifier("LM réajusté : x1 change de signe, toujours significatif, hors de son ancien intervalle",
         drapeau(C, "coef.x1", "signe") && !drapeau(C, "coef.x1", "seuil") && drapeau(C, "coef.x1", "ic") &&
           contient(phrase(C, "coef.x1"), "change de signe, toujours significatif (p < 0,001)"))
verifier("LM réajusté : x2 passe le seuil de 5 %, sans changer de signe", drapeau(C, "coef.x2", "seuil") && !drapeau(C, "coef.x2", "signe") &&
           contient(phrase(C, "coef.x2"), "et n'est plus significatif (p = "))
verifier("LM réajusté : le coefficient de la catégorie c apparaît, celui de b disparaît",
         drapeau(C, "coef.habitat(c)", "nouveau") && drapeau(C, "coef.habitat(b)", "disparu") && is.null(comparee(C, "coef.habitat(c)")$avant) &&
           contient(phrase(C, "coef.habitat(c)"), "apparaît") && contient(phrase(C, "coef.habitat(b)"), "disparaît (avant : "))
x1 <- comparee(C, "coef.x1")
verifier("LM réajusté : l'écart et l'écart relatif, la phrase de x1 avec les nombres du texte des résultats (virgule décimale)",
         proche(x1$ecart, x1$apres - x1$avant) && proche(x1$ecart_relatif, (x1$apres - x1$avant) / abs(x1$avant)) &&
           contient(x1$phrase, sprintf("passe de %s à %s", nombre_resultat(x1$avant, TRUE), nombre_resultat(x1$apres, TRUE))) && contient(x1$phrase, "IC 95 % ["))
verifier("LM réajusté : les comptes du résumé, les phrases de la plus importante à la moins importante",
         C$resume$signe == 1 && C$resume$seuil == 1 && C$resume$nouvelles == 1 && C$resume$disparues == 1 &&
           C$resume$changees + C$resume$identiques == C$resume$entrees && !is.unsorted(rev(vapply(C$phrases, `[[`, 0, "importance"))) &&
           identical(as.character(C$phrases[[1]]$cles)[1], "coef.x1"))
# le même résumé dans l'autre langue : les mêmes clés ; des phrases en anglais, un point décimal
ancien <- options(nuee.langue = "en")
Aen <- resume(pA, dA, mA)
Ben <- resume(pB, lm_de(pB))
options(ancien)
Cen <- comparer_resultats(Aen, Ben, "en")
verifier("LM en anglais : les mêmes clés, des libellés et des phrases en anglais (point décimal)",
         identical(cles(Aen), cles(A)) && identical(entree(Aen, "coef.x1")$libelle, "Coefficient of “x1”") &&
           contient(phrase(Cen, "coef.x1"), sprintf("goes from %s to %s (95%% CI [", nombre_resultat(x1$avant, FALSE), nombre_resultat(x1$apres, FALSE))) &&
           contient(phrase(Cen, "coef.x1"), "changing sign, still significant (p < 0.001)") && contient(phrase(Cen, "coef.x2"), "no longer significant") &&
           contient(phrase(Cen, "coef.habitat(c)"), "is new:") && contient(phrase(Cen, "coef.habitat(b)"), "is gone (before: "))
verifier("un résumé passé par JSON (ce que l'interface renvoie) se compare comme l'original : rien ne change, les mêmes drapeaux ensuite",
         comparer_resultats(par_json(list(modele = "M1", entrees = A)), par_json(A), "fr")$resume$changees == 0 &&
           identical(lapply(comparer_resultats(par_json(A), par_json(B), "fr")$entrees, `[`, c("cle", "signe", "seuil", "ic", "nouveau", "disparu", "phrase")),
                     lapply(C$entrees, `[`, c("cle", "signe", "seuil", "ic", "nouveau", "disparu", "phrase"))))

# un GLM de Poisson, un modèle mixte avec une pente aléatoire et CAR1, un GAM avec une courbe
glm_de <- function(p) definir_modele(p, "GLM", x = numeros(p, c("x1", "x2")), y = numeros(p, "comptes"), observations = numeros_observations(p), echelle = c(defaut = "none"),
                                     famille = "poisson")
mg <- ajuster(pA, glm_de(pA))
G <- resume(pA, glm_de(pA), mg)
verifier("GLM : les références trouvées, la déviance expliquée en pourcentage, pas de R² (que le document n'offre pas), la dispersion",
         bien_forme(G) && trouvees(G, references_regression(mg)) && is.null(entree(G, "R2")) && identical(entree(G, "deviance_expliquee")$forme, "pourcentage") &&
           proche(valeur(G, "deviance_expliquee"), mg$resultat$r2$deviance_expliquee) && !is.null(entree(G, "dispersion")))
CG <- comparer_resultats(G, resume(pB, glm_de(pB)), "fr")
verifier("GLM réajusté : l'effet de x1 change de signe", drapeau(CG, "coef.x1", "signe"))
lmm_de <- function(p) definir_modele(p, "LMM", x = numeros(p, c("annee", "x1")), y = numeros(p, "y"), observations = numeros_observations(p), echelle = c(defaut = "Ctr"),
                                     aleatoires = list(list(by = "variable", variable = numeros(p, "site"), pentes = numeros(p, "annee"))),
                                     correlation = list(type = "CAR1", temps = list(by = "variable", variable = numeros(p, "annee"))))
ml <- ajuster(pA, lmm_de(pA))
L <- resume(pA, lmm_de(pA), ml)
v <- ml$resultat$variances
verifier("LMM : R² marginal et conditionnel, l'écart-type du site, de sa pente et de la résiduelle, leur corrélation, φ de CAR1",
         bien_forme(L) && trouvees(L, references_regression(ml)) && proche(valeur(L, "sigma.site"), sqrt(v$variance[v$composante == "g1" & v$terme == "(Intercept)"])) &&
           proche(valeur(L, "sigma.site.annee"), sqrt(v$variance[v$composante == "g1" & v$terme != "(Intercept)"])) && !is.null(entree(L, "sigma.residuelle")) &&
           !is.null(entree(L, "rho.site.annee")) && proche(valeur(L, "phi"), ml$resultat$phi) && identical(entree(L, "sigma.site")$groupe, "aleatoires"))
ancien <- options(nuee.langue = "en")
verifier("LMM : les clés des effets aléatoires ne changent pas avec la langue (la résiduelle a son nom traduit dans les tableaux)",
         identical(cles(resume(pA, lmm_de(pA), ml)), cles(L)))
options(ancien)
# une courbe nette d'abord, puis aucune : son test passe le seuil
tableau_gam <- function(graine, forte) {
  set.seed(graine)
  x <- stats::runif(80, 0, 10)
  data.frame(nom = sprintf("g%02d", 1:80), x = x, z = stats::rnorm(80), y = 1 + (if (forte) 2 * sin(x) else 0) + stats::rnorm(80, 0, 0.5), stringsAsFactors = FALSE)
}
pgA <- projet_depuis_tableau(tableau_gam(3, TRUE), "gam")
pgB <- projet_depuis_tableau(tableau_gam(4, FALSE), "gam")
gam_de <- function(p) definir_modele(p, "GAM", x = numeros(p, c("x", "z")), y = numeros(p, "y"), observations = numeros_observations(p), echelle = c(defaut = "none"),
                                     lisses = numeros(p, "x"))
ga <- ajuster(pgA, gam_de(pgA))
GA <- resume(pgA, gam_de(pgA), ga)
CGa <- comparer_resultats(GA, resume(pgB, gam_de(pgB)), "fr")
verifier("GAM : la courbe de x, son edf et son test ; réajusté sans effet, la courbe n'est plus significative",
         bien_forme(GA) && trouvees(GA, references_regression(ga)) && proche(valeur(GA, "courbe.x"), ga$resultat$courbes$edf[1]) &&
           drapeau(CGa, "courbe.x", "seuil") && contient(phrase(CGa, "courbe.x"), "et n'est plus significative"))
# un jeu de test, des moyennes ajustées et la puissance d'une tendance gardées avec la régression
dt <- definir_modele(pA, "LM", x = numeros(pA, c("x1", "habitat")), y = numeros(pA, "y"), observations = numeros_observations(pA)[1:48], echelle = c(defaut = "none"),
                     test = numeros_observations(pA)[49:60])
mt <- ajuster(pA, dt)
mt$moyennes <- moyennes_ajustees(pA, mt, dt, list())
TJ <- resume(pA, dt, mt)
mo <- mt$moyennes$moyennes
verifier("LM avec un jeu de test et ses moyennes ajustées : RMSEP et Q² externe, chaque moyenne avec son intervalle, chaque paire avec sa valeur p",
         bien_forme(TJ) && proche(valeur(TJ, "jeu_test.RMSEP"), mt$test$statistiques$RMSEP) && proche(valeur(TJ, "jeu_test.Q2"), mt$test$statistiques$Q2ext) &&
           proche(valeur(TJ, "moyenne.habitat.a"), mo$estimation[mo$niveau == "a"]) && proche(entree(TJ, "moyenne.habitat.a")$ic, c(mo$bas[mo$niveau == "a"], mo$haut[mo$niveau == "a"])) &&
           sum(startsWith(cles(TJ), "paire.habitat.")) == nrow(mt$moyennes$paires) && !is.null(entree(TJ, "paire.habitat.a − b")$p))
dp <- definir_modele(pA, "LMM", x = numeros(pA, "annee"), y = numeros(pA, "y"), observations = numeros_observations(pA), echelle = c(defaut = "none"),
                     aleatoires = list(list(by = "variable", variable = numeros(pA, "site"))))
mp <- ajuster(pA, dp)
mp$puissance_tendance <- puissance_tendance(mp, dp)
P <- resume(pA, dp, mp)
pt <- mp$puissance_tendance
ici <- pt$tableau[pt$tableau$unites == pt$plan$unites & pt$tableau$annees == pt$plan$annees, ]
verifier("LMM avec la puissance de sa tendance : la puissance sur les années des données, les années qu'il faut pour 80 % (quand elles y suffisent)",
         proche(valeur(P, "puissance"), ici$puissance) && identical(entree(P, "puissance")$groupe, "puissance") &&
           identical(is.na(pt$annees_80$annees[pt$annees_80$unites == pt$plan$unites]), is.null(entree(P, "puissance.annees_80"))))

# ---------------------------------------------------------------- ACP, PLS, OPLS, PLS-DA

cat("Modèles de composantes : l'exemple, puis des données changées\n")
pe <- lire_projet("exemple/vergers.json")
for (k in seq_along(pe$models)) {
  d <- modele_du_projet(pe, k)
  m <- ajuster(pe, d)
  E <- resume(pe, d, m)
  cp <- m$statistiques$composantes
  type <- paste0(if (isTRUE(m$opls)) "O" else "", if (isTRUE(m$plsda)) "PLS-DA" else m$type)
  ok_valeurs <- proche(valeur(E, "Q2"), utils::tail(cp$Q2cum, 1)) && proche(valeur(E, "R2X"), utils::tail(cp$R2Xcum, 1)) &&
    proche(valeur(E, sprintf("Q2.%d", nrow(cp))), utils::tail(cp$Q2cum, 1)) && proche(valeur(E, "A"), m$A)
  if (identical(m$type, "PLS")) {
    co <- m$statistiques$coefficients
    y1 <- m$y$noms[1]
    t1 <- co$terme[1]
    ok_valeurs <- ok_valeurs && proche(valeur(E, paste0("vip.", t1)), m$statistiques$variables$VIP[1]) && proche(valeur(E, paste0("coef.", t1)), co[[paste0("CoeffCS ", y1)]][1]) &&
      proche(entree(E, paste0("coef.", t1))$ic, co[[paste0("CoeffCS ", y1)]][1] + c(-1, 1) * co[[paste0("CoeffCS ", y1, " IC")]][1]) &&
      proche(valeur(E, "cvanova.F"), m$statistiques$cv_anova[[1]]$F[2]) && proche(entree(E, "cvanova.F")$p, m$statistiques$cv_anova[[1]]$p[2])
  }
  verifier(sprintf("%s (%s) : un résumé bien formé, chaque référence d'un document trouvée, les valeurs du modèle", d$name, type),
           bien_forme(E) && trouvees(E, references_composantes(m)) && ok_valeurs)
}
# un modèle importé d'un fichier .usp (son logiciel, ses résultats enregistrés) : ses résultats enregistrés aussi
du <- modele_du_projet(pe, 2)
du$version_origine <- 11
du$reference <- list(components = lapply(seq_len(du$A), function(a) list(`R2X(cum)` = 0.5 + a / 100, `R2Y(cum)` = 0.6 + a / 100, `Q2(cum)` = 0.4 + a / 100)))
U <- resume(pe, du)
verifier("modèle d'un fichier .usp : ses résultats refaits par Nuée, et ceux que le fichier a enregistrés pour son nombre de composantes",
         bien_forme(U) && trouvees(U, references_composantes(ajuster(pe, du))) && proche(valeur(U, "stocke.Q2"), 0.4 + du$A / 100) &&
           proche(valeur(U, "stocke.R2Y"), 0.6 + du$A / 100) && identical(entree(U, "stocke.R2X")$groupe, "stocke"))

# deux versions d'un tableau de 8 variables : l'effet de x3 sur y change de signe, x5 devient
# importante (son VIP passe 1) ; une ACP à 2 composantes, puis à 3
tableau_pls <- function(graine, b3, b5) {
  set.seed(graine)
  n <- 50
  z <- stats::rnorm(n)
  X <- sapply(1:8, function(j) z * (j <= 4) + stats::rnorm(n))
  colnames(X) <- sprintf("x%d", 1:8)
  y <- 0.8 * X[, 1] + b3 * X[, 3] + b5 * X[, 5] + stats::rnorm(n, 0, 0.3)
  data.frame(nom = sprintf("p%02d", 1:n), X, y = y, classe = ifelse(y > stats::median(y), "haut", "bas"), stringsAsFactors = FALSE)
}
ppA <- projet_depuis_tableau(tableau_pls(5, 0.8, 0), "pls")
ppB <- projet_depuis_tableau(tableau_pls(6, -0.8, 1.2), "pls")
xs <- sprintf("x%d", 1:8)
pls_de <- function(p, A = 2) definir_modele(p, "PLS", x = numeros(p, xs), y = numeros(p, "y"), A = A)
CP <- comparer_resultats(resume(ppA, pls_de(ppA)), resume(ppB, pls_de(ppB)), "fr")
verifier("PLS réajustée : le coefficient de x3 change de signe, significatif (son intervalle jack-knife exclut 0)",
         drapeau(CP, "coef.x3", "signe") && contient(phrase(CP, "coef.x3"), "change de signe"))
verifier("PLS réajustée : le VIP de x5 passe 1 (son repère), dit dans la phrase", drapeau(CP, "vip.x5", "seuil") && contient(phrase(CP, "vip.x5"), "désormais au-dessus de 1"))
CP3 <- comparer_resultats(resume(ppA, pls_de(ppA, 2)), resume(ppB, pls_de(ppB, 3)), "fr")
verifier("PLS à une composante de plus : les valeurs de la composante 3 sont nouvelles, A le dit dans une phrase, sans phrase par composante",
         drapeau(CP3, "Q2.3", "nouveau") && drapeau(CP3, "R2X.3", "nouveau") && is.null(comparee(CP3, "Q2.3")$phrase) &&
           identical(phrase(CP3, "A"), "Le nombre de composantes passe de 2 à 3."))
acp_de <- function(p, A) definir_modele(p, "PCA", x = numeros(p, xs), A = A)
ma <- ajuster(ppA, acp_de(ppA, 2))
AC <- resume(ppA, acp_de(ppA, 2), ma)
v <- ma$statistiques$variables
verifier("ACP : chaque loading avec son intervalle jack-knife et sa composante (axe), le R²V de chaque variable, pas de VIP",
         bien_forme(AC) && trouvees(AC, references_composantes(ma)) && proche(valeur(AC, "p1.x2"), v$p1[2]) && proche(entree(AC, "p1.x2")$ic, v$p1[2] + c(-1, 1) * v$p1_IC[2]) &&
           identical(entree(AC, "p2.x2")$axe, "2") && proche(valeur(AC, "R2V.x2"), v$R2V_cum[2]) && !any(startsWith(cles(AC), "vip.")))
CA3 <- comparer_resultats(AC, resume(ppB, acp_de(ppB, 3)), "fr")
verifier("ACP à une composante de plus : les loadings p3 et le Q² de la composante 3 apparaissent",
         drapeau(CA3, "p3.x1", "nouveau") && drapeau(CA3, "Q2.3", "nouveau") && CA3$resume$nouvelles == 8 + 2)
# une composante retournée (le signe d'une composante est arbitraire) : remise dans le sens d'avant
retournee <- lapply(AC, function(e) {
  if (identical(e$axe, "1")) {
    e$valeur <- -e$valeur
    e$ic <- sort(-e$ic)
  }
  e
})
CR <- comparer_resultats(AC, retournee, "fr")
verifier("ACP dont la composante 1 sort retournée : aucun changement de signe, ses loadings marqués retournés, une phrase le dit",
         CR$resume$retournees == 1 && CR$resume$signe == 0 && drapeau(CR, "p1.x1", "retourne") && !drapeau(CR, "p2.x1", "retourne") && CR$resume$changees == 0 &&
           any(vapply(CR$phrases, function(f) contient(f$texte, "La composante 1 est sortie retournée"), TRUE)))
da_de <- function(p) definir_modele(p, "PLS-DA", x = numeros(p, xs), classe = numeros(p, "classe"), A = 2)
mda <- ajuster(ppA, da_de(ppA))
DA <- resume(ppA, da_de(ppA), mda)
verifier("PLS-DA : la part bien classée en validation croisée, au total et par classe avec son effectif",
         bien_forme(DA) && trouvees(DA, references_composantes(mda)) && proche(valeur(DA, "bien_classes"), mda$statistiques$classification$vc$global) &&
           !is.null(entree(DA, paste0("bien_classes.", mda$y$noms[1]))$n))
dop <- definir_modele(ppA, "OPLS", x = numeros(ppA, xs), y = numeros(ppA, "y"), A = 2)
mop <- ajuster(ppA, dop)
OP <- resume(ppA, dop, mop)
verifier("OPLS : ses composantes, ses VIP et ses coefficients", bien_forme(OP) && trouvees(OP, references_composantes(mop)) && proche(valeur(OP, "A"), 2))
# beaucoup de variables qui changent : une phrase par drapeau, qui les compte, au lieu d'une chacune
tableau_longueurs <- function(graine, signe) {
  set.seed(graine)
  n <- 40
  z <- stats::rnorm(n)
  X <- sapply(1:30, function(j) z * sin(j / 5) + stats::rnorm(n, 0, 0.3))
  colnames(X) <- sprintf("l%02d", 1:30)
  data.frame(nom = sprintf("s%02d", 1:n), X, y = signe * z + stats::rnorm(n, 0, 0.2), stringsAsFactors = FALSE)
}
psA <- projet_depuis_tableau(tableau_longueurs(7, 1), "spectres")
psB <- projet_depuis_tableau(tableau_longueurs(8, -1), "spectres")
spectre_de <- function(p) definir_modele(p, "PLS", x = numeros(p, sprintf("l%02d", 1:30)), y = numeros(p, "y"), A = 1)
CS <- comparer_resultats(resume(psA, spectre_de(psA)), resume(psB, spectre_de(psB)), "fr")
resumees <- Filter(function(f) length(f$cles) > 1, CS$phrases)
verifier(sprintf("PLS de 30 variables dont les coefficients changent de signe : une phrase qui les compte (%s)", if (length(resumees)) resumees[[1]]$texte else "aucune"),
         CS$resume$signe >= 20 && length(resumees) >= 1 && any(vapply(resumees, function(f) grepl("^[0-9]+ valeurs changent de signe : ", f$texte), TRUE)) &&
           all(vapply(CS$phrases, function(f) length(f$cles) > 1 || !all(startsWith(as.character(f$cles), "coef.")), TRUE)))

# ---------------------------------------------------------------- les autres analyses

cat("Ordinations et communautés : RDA, CA, CCA, PCoA, NMDS, PERMANOVA, Mantel, espèces indicatrices\n")
# 40 relevés de 10 espèces le long d'un gradient de température ; dans la version d'après, la
# température ne joue plus (les abondances au hasard), et sp10 n'est plus vue nulle part
tableau_especes <- function(graine, gradient) {
  set.seed(graine)
  n <- 40
  g <- stats::runif(n, -2, 2)
  habitat <- rep(c("foret", "marais", "prairie"), length.out = n)
  effet <- c(foret = -0.5, marais = 0.7, prairie = 0)
  mu <- sapply(1:10, function(j) exp(0.8 + gradient * sin(j) * g + gradient * effet[habitat] * ((j %% 3) - 1)))
  ab <- matrix(stats::rpois(n * 10, mu), n, 10, dimnames = list(NULL, sprintf("sp%02d", 1:10)))
  ab[rowSums(ab) == 0, 1] <- 1
  data.frame(nom = sprintf("r%02d", 1:n), temp = g * 3 + 12 + stats::rnorm(n, 0, 0.3), ph = stats::rnorm(n, 6.5, 0.3), habitat = habitat,
             site = rep(sprintf("S%d", 1:8), each = 5), ab, stringsAsFactors = FALSE)
}
peA <- projet_depuis_tableau(tableau_especes(11, 1), "relevés")
dfB <- tableau_especes(12, 0)
dfB$sp10 <- 0
peB <- projet_depuis_tableau(dfB, "relevés")
esp <- sprintf("sp%02d", 1:10)
ordination <- function(type, options, x = c("temp", "ph", "habitat"), y = esp, classe = NULL) function(p) {
  definir_modele(p, type, x = if (length(x)) numeros(p, x), y = if (length(y)) numeros(p, y), classe = if (length(classe)) numeros(p, classe), echelle = "none", options = options)
}
analyses <- list(
  RDA = ordination("RDA", list(permutations = 99, graine = 1, transformation = "hellinger")),
  CCA = ordination("CCA", list(permutations = 99, graine = 2), y = esp[1:9]),
  PCOA = ordination("PCOA", list(distance = "bray", permutations = 99, graine = 3)),
  NMDS = ordination("NMDS", list(distance = "bray", k = 2, departs = 5, graine = 4, permutations = 99), x = c("temp", "habitat")),
  PERMANOVA = ordination("PERMANOVA", list(distance = "bray", permutations = 99, graine = 5, par = "termes"), x = c("temp", "habitat")),
  MANTEL = ordination("MANTEL", list(distance = "bray", permutations = 99, graine = 6, z = NULL), x = "temp"))
attendus <- list(RDA = c("R2", "R2_ajuste", "test.modele", "axe.RDA1"), CCA = c("R2", "R2_ajuste", "test.modele", "axe.CCA1"),
                 PCOA = c("axe.PCoA1", "ajuste.temp", "ajuste.habitat"), NMDS = c("stress", "ajuste.temp", "ajuste.habitat"),
                 PERMANOVA = c("permanova.modele", "permanova.temp", "permanova.habitat", "dispersion.habitat"), MANTEL = "mantel")
seuils <- list(RDA = "test.modele", CCA = "test.modele", PCOA = "ajuste.temp", NMDS = "ajuste.temp", PERMANOVA = "permanova.temp", MANTEL = "mantel")
for (type in names(analyses)) {
  d <- analyses[[type]](peA)
  m <- ajuster(peA, d)
  E <- resume(peA, d, m)
  r <- interface_autre(peA, d, m)
  CO <- comparer_resultats(E, resume(peB, analyses[[type]](peB)), "fr")
  verifier(sprintf("%s : un résumé bien formé (%s), les références d'un document trouvées ; la température ne jouant plus, « %s » passe le seuil de 5 %%",
                   type, paste(attendus[[type]], collapse = ", "), seuils[[type]]),
           bien_forme(E) && all(attendus[[type]] %in% cles(E)) && trouvees(E, references_autre(r)) && memes_valeurs_autre(E, r) &&
             drapeau(CO, seuils[[type]], "seuil") && nzchar(phrase(CO, seuils[[type]])))
}
dr <- analyses$RDA(peA)
mr <- ajuster(peA, dr)
R <- resume(peA, dr, mr)
verifier("RDA : R², R² ajusté, le pseudo-F et la valeur p du modèle, la part de l'inertie de RDA1, ceux du modèle",
         proche(valeur(R, "R2"), mr$r2) && proche(valeur(R, "R2_ajuste"), mr$r2_ajuste) && proche(valeur(R, "test.modele"), mr$test$F) &&
           proche(entree(R, "test.modele")$p, mr$test$p) && proche(valeur(R, "axe.RDA1"), mr$noyau$contraint$eig[["RDA1"]] / mr$noyau$totale))
dca <- ordination("CA", list(), x = esp, y = NULL)(peA)
mca <- ajuster(peA, dca)
CAe <- resume(peA, dca, mca)
CCa <- comparer_resultats(CAe, resume(peB, ordination("CA", list(), x = esp, y = NULL)(peB)), "fr")
verifier("CA : l'inertie totale et la part de ses premiers axes ; sp10, vide après, laisse le tableau d'une colonne de moins",
         bien_forme(CAe) && proche(valeur(CAe, "axe.CA1"), mca$noyau$eig[[1]] / mca$noyau$totale) && proche(valeur(CAe, "inertie.totale"), mca$noyau$totale) &&
           trouvees(CAe, references_autre(interface_autre(peA, dca, mca))) && memes_valeurs_autre(CAe, interface_autre(peA, dca, mca)) &&
           identical(comparee(CCa, "K")$ecart, -1) && contient(phrase(CCa, "K"), "Le nombre de colonnes du tableau passe de 10 à 9."))
indval <- ordination("INDVAL", list(permutations = 199, graine = 7, ordre = 1), x = NULL, classe = "habitat")
mi <- ajuster(peA, indval(peA))
I <- resume(peA, indval(peA), mi)
CI <- comparer_resultats(I, resume(peB, indval(peB)), "fr")
groupe_change <- Filter(function(x) isTRUE(x$categorie) && !is.null(x$phrase), CI$entrees)
sans_portee <- Filter(function(x) isTRUE(x$categorie) && !isTRUE(x$seuil) && isTRUE(x$p_avant > 0.05) && isTRUE(x$p_apres > 0.05), CI$entrees)
verifier("espèces indicatrices : chaque espèce avec sa valeur, sa valeur p corrigée, son groupe ; sp10, absente après, disparaît ; des espèces changent de groupe",
         bien_forme(I) && trouvees(I, references_autre(interface_autre(peA, indval(peA), mi))) && memes_valeurs_autre(I, interface_autre(peA, indval(peA), mi)) &&
           proche(valeur(I, "indval.sp01"), mi$valeur[1]) && proche(entree(I, "indval.sp01")$p, mi$p_ajuste[1]) &&
           identical(entree(I, "indval.sp01")$classe, colnames(mi$combinaisons)[mi$meilleure[1]]) && drapeau(CI, "indval.sp10", "disparu") &&
           length(groupe_change) > 0 && contient(groupe_change[[1]]$phrase, " avant, « "))
verifier("espèces indicatrices : le groupe d'une espèce qui n'est indicatrice ni avant ni après change sans phrase (il n'a pas de portée)",
         length(sans_portee) > 0 && all(vapply(sans_portee, function(x) is.null(x$phrase), TRUE)))
verifier("espèces indicatrices : des espèces indicatrices avant ne le sont plus après", CI$resume$seuil > 0 && valeur(I, "indicatrices") > 0)

cat("TRIM et l'échantillonnage par distances\n")
# un déclin, puis une hausse et une année de plus
tableau_trim <- function(graine, pente, J) {
  set.seed(graine)
  I <- 20
  g <- expand.grid(j = seq_len(J), i = seq_len(I))
  compte <- stats::rpois(nrow(g), exp(stats::rnorm(I, 2.5, 0.6)[g$i] + pente * g$j))
  compte[stats::runif(nrow(g)) < 0.15] <- NA
  data.frame(nom = sprintf("t%03d", seq_len(nrow(g))), site = sprintf("S%02d", g$i), annee = 2000 + g$j - 1, compte = compte, stringsAsFactors = FALSE)
}
ptA <- projet_depuis_tableau(tableau_trim(13, -0.08, 10), "trim")
ptB <- projet_depuis_tableau(tableau_trim(14, 0.08, 11), "trim")
trim_de <- function(p, modele = 2) {
  o <- list(site = list(by = "variable", variable = numeros(p, "site")), temps = list(by = "variable", variable = numeros(p, "annee")), modele = modele,
            surdispersion = TRUE, correlation = TRUE, changements = if (modele == 2) c(2000, 2004))
  definir_modele(p, "TRIM", x = NULL, y = numeros(p, "compte"), echelle = "none", options = o)
}
mtA <- ajuster(ptA, trim_de(ptA))
TA <- resume(ptA, trim_de(ptA), mtA)
imputee <- mtA$pentes[mtA$pentes$quoi == "imputee", ]
verifier("TRIM : la pente globale, son intervalle, sa valeur p et sa classe ; les indices (± 1,96 erreur-type), les pentes entre les points de changement, les tests de Wald",
         bien_forme(TA) && trouvees(TA, references_autre(interface_autre(ptA, trim_de(ptA), mtA))) && memes_valeurs_autre(TA, interface_autre(ptA, trim_de(ptA), mtA)) &&
           proche(valeur(TA, "pente"), imputee$multiplicative) && proche(entree(TA, "pente")$ic, c(imputee$bas, imputee$haut)) &&
           identical(entree(TA, "pente")$classe, imputee$classe) &&
           proche(entree(TA, "indice.2003")$ic, mtA$indices$impute[4] + c(-1, 1) * stats::qnorm(0.975) * mtA$indices$se_impute[4]) && is.null(entree(TA, "indice.2000")) &&
           !is.null(entree(TA, "pente.2000-2004")) && !is.null(entree(TA, "wald.changement.2004")))
CT <- comparer_resultats(TA, resume(ptB, trim_de(ptB)), "fr")
classes <- c(comparee(CT, "pente")$classe_avant, comparee(CT, "pente")$classe_apres)
verifier(sprintf("TRIM réajusté : la pente passe de l'autre côté de 1, sa classe change, l'indice de 2010 apparaît (%s)", phrase(CT, "pente")),
         drapeau(CT, "pente", "signe") && drapeau(CT, "pente", "categorie") && drapeau(CT, "indice.2010", "nouveau") && identical(comparee(CT, "temps")$ecart, 1) &&
           identical(classes[1], imputee$classe) && contient(phrase(CT, "pente"), "désormais au-dessus de 1") &&
           contient(phrase(CT, "pente"), sprintf("« %s » avant, « %s » maintenant", libelle_classe_trim(classes[1], TRUE), libelle_classe_trim(classes[2], TRUE))))
ancien <- options(nuee.langue = "en")
CTen <- comparer_resultats(resume(ptA, trim_de(ptA), mtA), resume(ptB, trim_de(ptB)), "en")
options(ancien)
verifier("TRIM en anglais : la classe de la tendance en mots anglais",
         contient(phrase(CTen, "pente"), sprintf("“%s” before, “%s” now", libelle_classe_trim(classes[1], FALSE), libelle_classe_trim(classes[2], FALSE))) &&
           contient(phrase(CTen, "pente"), "The overall slope (imputed totals) goes from"))
verifier("TRIM, modèle 3 : les tests des écarts à la droite", bien_forme(resume(ptA, trim_de(ptA, 3))) && !is.null(entree(resume(ptA, trim_de(ptA, 3)), "wald.deviations")))
# deux strates de transects, puis une troisième
tableau_distance <- function(graine, strates) {
  set.seed(graine)
  do.call(rbind, lapply(seq_len(8 * strates), function(j) {
    x <- stats::runif(stats::rpois(1, 40), 0, 100)
    vu <- stats::runif(length(x)) < exp(-(x / 30)^2 / 2)
    data.frame(strate = c("Nord", "Sud", "Est")[(j - 1) %/% 8 + 1], aire = 40, ligne = sprintf("L%02d", j), effort = 2, distance = round(x[vu], 1))
  }))
}
distance_de <- function(p) {
  o <- list(transect = "ligne", echantillon = list(by = "variable", variable = numeros(p, "ligne")), strate = list(by = "variable", variable = numeros(p, "strate")),
            effort = numeros(p, "effort"), aire = numeros(p, "aire"), troncature = list(type = "distance", valeur = 100), cle = "hn", serie = "cos", ajustements = "aic",
            monotonie = "stricte", unites = list(distance = "m", effort = "km", aire = "km2"))
  definir_modele(p, "DISTANCE", x = NULL, y = numeros(p, "distance"), echelle = "none", options = o)
}
dd <- tableau_distance(15, 2)
pdA <- projet_depuis_tableau(cbind(nom = sprintf("d%04d", seq_len(nrow(dd))), dd, stringsAsFactors = FALSE), "distances")
dd <- tableau_distance(16, 3)
pdB <- projet_depuis_tableau(cbind(nom = sprintf("d%04d", seq_len(nrow(dd))), dd, stringsAsFactors = FALSE), "distances")
mdA <- ajuster(pdA, distance_de(pdA))
DS <- resume(pdA, distance_de(pdA), mdA)
tot <- mdA$densite[nrow(mdA$densite), ]
verifier("échantillonnage par distances : la fonction de détection retenue (une catégorie), p, ESW, la densité totale et celle de chaque strate avec leurs intervalles, l'adéquation",
         bien_forme(DS) && trouvees(DS, references_autre(interface_autre(pdA, distance_de(pdA), mdA))) && memes_valeurs_autre(DS, interface_autre(pdA, distance_de(pdA), mdA)) &&
           identical(entree(DS, "modele")$classe, libelle_detection(mdA$detection$fd)) && is.null(entree(DS, "modele")$valeur) &&
           proche(valeur(DS, "D"), tot$D) && proche(entree(DS, "D")$ic, c(tot$bas_D, tot$haut_D)) && !is.null(entree(DS, "D.Nord")) && proche(valeur(DS, "p"), mdA$p) &&
           !is.null(entree(DS, "ESW")) && !is.null(entree(DS, "adequation.khi2")$p))
CD <- comparer_resultats(DS, resume(pdB, distance_de(pdB)), "fr")
verifier("échantillonnage par distances réajusté : la densité de la strate Est apparaît", drapeau(CD, "D.Est", "nouveau") && contient(phrase(CD, "D.Est"), "La densité de la strate « Est » apparaît"))

cat("Forêts, arbres boostés, arbres, LDA et k-moyennes\n")
# la densité dépend du gel, puis des haies
tableau_ens <- function(graine, gel) {
  set.seed(graine)
  n <- 160
  site <- rep(sprintf("S%02d", 1:20), each = 8)
  X <- data.frame(h_gel = round(stats::runif(n, 0, 30)), t_moy = stats::rnorm(n, 12, 2), haie = stats::runif(n), sol = sample(c("argile", "limon", "sable"), n, TRUE), stringsAsFactors = FALSE)
  y <- 2 + (if (gel) ifelse(X$h_gel > 12, -1.2, 0) else 1.5 * (X$haie > 0.5)) + stats::rnorm(n, 0, 0.3)
  data.frame(nom = sprintf("e%03d", seq_len(n)), site = site, X, densite = y, classe = ifelse(y > 2.2, "haute", ifelse(y > 1.4, "moyenne", "basse")),
             presence = as.numeric(y > 1.8), stringsAsFactors = FALSE)
}
pfA <- projet_depuis_tableau(tableau_ens(17, TRUE), "ensembles")
pfB <- projet_depuis_tableau(tableau_ens(18, FALSE), "ensembles")
xe <- c("h_gel", "t_moy", "haie", "sol")
ensemble <- function(type, y = "densite", classe = NULL, options = list(), x = xe, echelle = "none") function(p) {
  definir_modele(p, type, x = numeros(p, x), y = if (length(y)) numeros(p, y), classe = if (length(classe)) numeros(p, classe), echelle = echelle,
                 vc = list(by = "variable", variable = numeros(p, "site")), options = options)
}
modeles <- list(FOREST = ensemble("FOREST", options = list(arbres = 100)), FOREST_classes = ensemble("FOREST", y = NULL, classe = "classe", options = list(arbres = 100)),
                BOOST = ensemble("BOOST", options = list(arbres_max = 100, taux = 0.1)), BOOST_presence = ensemble("BOOST", y = "presence", options = list(arbres_max = 100, taux = 0.1, loi = "binomiale")),
                TREE = ensemble("TREE"), TREE_classes = ensemble("TREE", y = NULL, classe = "classe"),
                LDA = ensemble("LDA", y = NULL, classe = "classe", echelle = "UV"), KMEANS = ensemble("KMEANS", y = NULL, options = list(k = 3), x = c("h_gel", "t_moy", "haie"), echelle = "UV"))
attendus <- list(FOREST = c("R2_oob", "Q2", "RMSEcv", "importance.h_gel"), FOREST_classes = c("bien_classes_oob", "bien_classes", "importance.h_gel"),
                 BOOST = c("arbres", "D2", "D2_vc", "R2", "Q2", "RMSEE", "influence.h_gel"), BOOST_presence = c("arbres", "D2", "AUC", "AUC_vc", "bien_classes", "influence.h_gel"),
                 TREE = c("feuilles", "R2", "Q2", "RMSEE", "premiere_coupure", "importance.h_gel"), TREE_classes = c("feuilles", "bien_classes_ajuste", "bien_classes", "premiere_coupure"),
                 LDA = c("bien_classes_ajuste", "bien_classes", "bien_classes.haute", "axe.LD1"), KMEANS = c("k", "R2", "silhouette"))
for (nom in names(modeles)) {
  d <- modeles[[nom]](pfA)
  m <- ajuster(pfA, d)
  E <- resume(pfA, d, m)
  r <- interface_autre(pfA, d, m)
  verifier(sprintf("%s : un résumé bien formé (%s), les références d'un document trouvées (%s), avec les valeurs de l'interface",
                   nom, paste(attendus[[nom]], collapse = ", "), paste(references_autre(r), collapse = ", ")),
           bien_forme(E) && all(attendus[[nom]] %in% cles(E)) && trouvees(E, references_autre(r)) && memes_valeurs_autre(E, r))
}
CTr <- comparer_resultats(resume(pfA, modeles$TREE(pfA)), resume(pfB, modeles$TREE(pfB)), "fr")
verifier(sprintf("arbre réajusté : sa première coupure passe du gel aux haies (%s)", phrase(CTr, "premiere_coupure")),
         drapeau(CTr, "premiere_coupure", "categorie") && contient(phrase(CTr, "premiere_coupure"), "La variable de la première coupure passe de « h_gel » à « haie »."))
CF <- comparer_resultats(resume(pfA, modeles$FOREST(pfA)), resume(pfB, modeles$FOREST(pfB)), "fr")
verifier(sprintf("forêt réajustée : la variable la plus importante change (%s) ; l'importance de chacune change, sans phrase (pas de drapeau), avec son importance",
                 phrase(CF, "variable_principale")),
         drapeau(CF, "variable_principale", "categorie") && identical(phrase(CF, "variable_principale"), "La variable la plus importante (permutation) passe de « h_gel » à « haie ».") &&
           comparee(CF, "importance.h_gel")$change && is.null(comparee(CF, "importance.h_gel")$phrase) && comparee(CF, "importance.haie")$importance > 0)
CB <- comparer_resultats(resume(pfA, modeles$BOOST(pfA)), resume(pfB, modeles$BOOST(pfB)), "fr")
verifier("arbres boostés réajustés : la variable la plus influente change", drapeau(CB, "variable_principale", "categorie"))
dtt <- definir_modele(pfA, "TREE", x = numeros(pfA, xe), y = numeros(pfA, "densite"), echelle = "none", observations = 1:140, test = 141:160)
mtt <- ajuster(pfA, dtt)
verifier("arbre avec un jeu de test : RMSEP, Q² externe et biais", proche(valeur(resume(pfA, dtt, mtt), "jeu_test.RMSEP"), mtt$test$statistiques$RMSEP) &&
           !is.null(entree(resume(pfA, dtt, mtt), "jeu_test.biais")))

# ---------------------------------------------------------------- la comparaison elle-même

cat("La comparaison : des entrées d'un seul côté, des catégories seules, l'ordre, les références vides\n")
e1 <- list(entree_resultat("N", "Nombre d'observations", "ajustement", 10, forme = "entier"), entree_resultat("a", "Valeur a", "estimations", 1, ic = c(0.5, 1.5), limite = 0, genre = "f"),
           entree_resultat("b", "Valeur b", "estimations", 2), entree_resultat("c", "Modèle", "ajustement", classe = "hn"))
e2 <- list(entree_resultat("N", "Nombre d'observations", "ajustement", 12, forme = "entier"), entree_resultat("c", "Modèle", "ajustement", classe = "hr"),
           entree_resultat("a", "Valeur a", "estimations", 1.000000000001, ic = c(0.5, 1.5), limite = 0, genre = "f"), entree_resultat("d", "Valeur d", "estimations", 3))
C12 <- comparer_resultats(e1, e2, "fr")
verifier("l'ordre d'après, une entrée disparue à sa place d'avant ; un écart de l'ordre de l'arrondi n'est pas un changement",
         identical(vapply(C12$entrees, `[[`, "", "cle"), c("N", "c", "a", "b", "d")) && !comparee(C12, "a")$change && comparee(C12, "b")$disparu && comparee(C12, "d")$nouveau &&
           identical(ordre_comparaison(c("b", "x", "c", "y", "z"), c("a", "b", "c")), c("a", "b", "x", "c", "y", "z")) &&
           identical(ordre_comparaison(c("w", "b"), "b"), c("w", "b")) && identical(ordre_comparaison(c("a", "b"), c("b", "a")), c("b", "a")))
verifier("une catégorie seule qui change (le modèle) : sa phrase, ses deux catégories",
         drapeau(C12, "c", "categorie") && identical(phrase(C12, "c"), "Le modèle passe de « hn » à « hr ».") && identical(comparee(C12, "c")$classe_avant, "hn"))
verifier("un nombre entier qui change a sa phrase, sans décimale", identical(phrase(C12, "N"), "Le nombre d'observations passe de 10 à 12."))
verifier("assez de chiffres pour lire l'écart de deux valeurs proches, sans qu'un arrondi le grossisse ; 3 chiffres significatifs sinon",
         identical(ecriture_resultat(1234.51, 1236.84, NULL, TRUE)(1234.51), "1235") && identical(ecriture_resultat(1234.51, 1236.84, NULL, TRUE)(1236.84), "1237") &&
           identical(ecriture_resultat(0.6231, 0.6234, "pourcentage", FALSE)(0.6231), "62.31%") && identical(ecriture_resultat(0.808, -0.792, NULL, TRUE)(-0.792), "-0,792") &&
           identical(ecriture_resultat(0.62341, 0.62343, NULL, FALSE)(0.62343), "0.62343") && identical(ecriture_resultat(0.5, 0.75, "pourcentage", TRUE)(0.5), "50,0 %"))
verifier("rien d'un côté : toutes nouvelles ; rien de l'autre : toutes disparues ; rien des deux : vide",
         comparer_resultats(list(), e1, "fr")$resume$nouvelles == 4 && comparer_resultats(e1, list(), "fr")$resume$disparues == 4 &&
           comparer_resultats(list(), list(), "fr")$resume$entrees == 0)
verifier("une entrée sans valeur ni catégorie n'en est pas une", is.null(entree_resultat("x", "X", "ajustement", NA)) && is.null(entree_resultat("x", "X", "ajustement", NULL)))
verifier("une autre analyse sans entrees_<type> : le nombre de ses observations", length(entrees_communes(NULL, NULL, list(N = 7))) == 1 &&
           identical(entrees_communes(NULL, NULL, list(N = 7))[[1]]$valeur, 7))
verifier("chaque autre analyse a ses entrées (entrees_<type>)", all(vapply(AUTRES_ANALYSES, function(t) !is.null(fonction_analyse("entrees_", t)), TRUE)))

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
