# Vérifie les modèles écrits en code R (moteur/R/code.R, code_script.R) par les demandes du serveur,
# telles que l'interface les envoie (serveur.R : DEMANDES, chaque demande passée par le JSON) :
#   - lm, glm, nls et un modèle mixte de nlme : les mêmes coefficients, erreurs-types, valeurs p,
#     vraisemblance, AIC et BIC que ces fonctions appelées ici ; mgcv et MASS quand ils sont là ;
#   - les colonnes de coef(summary()) prises par leur nom : survival::coxph (se(coef), robust se) et
#     nnet::multinom (coef() et vcov()) quand ils sont là ; la loi t d'un gls (N − p) ;
#   - la forme liste : ses valeurs et ses tableaux, ses éléments inconnus ; ce que le code affiche, ses
#     messages, ses avertissements, un graphique dessiné ;
#   - une erreur et sa ligne, une erreur de lecture, un paquet qui manque, un code vide, une valeur NULL ;
#   - la confiance : un code jamais approuvé refusé (ajuster, l'export vers R), approuvé par la demande
#     code_approuve, que seule l'application fait quand elle a donné son secret au moteur ; un projet
#     enregistré puis changé ailleurs (son code autre) : non approuvé, refusé, dit dans son résumé ; la
#     politique qui retire le type ;
#   - les colonnes que le code nomme : la mise à jour des données dit le modèle qui en perd une ;
#   - Ce qui a changé : ses résultats clés avant et après une mise à jour des données ;
#   - le texte des méthodes, avec R, les paquets, leur version et leur citation ;
#   - la comparaison par l'AIC : le même ensemble que la régression linéaire de Nuée de la même réponse
#     sur les mêmes observations ; écarté, avec sa raison, ajusté par REML ;
#   - les prédictions, sur l'échelle de la réponse (un glm de Poisson : des comptages) ; les tableaux exportés ;
#   - l'export vers R : le script écrit, lancé seul par Rscript, redonne les mêmes nombres.
#   Rscript moteur/tests/verifier_code.R

options(warn = 1, stringsAsFactors = FALSE)
racine_nuee <- normalizePath(".", winslash = "/")
Sys.setenv(NUEE_CODE_SECRET = "secret-de-l-essai")  # (comme main.js : les approbations viennent de l'application)
Sys.unsetenv("NUEE_SANS_CODE")
# Le serveur sans son écoute (comme verifier_etapes.R), jusqu'à son interface HTTP comprise : chaque
# expression de serveur.R sauf le démarrage de httpuv, sa racine et ses arguments donnés ici.
local({
  assign("racine", racine_nuee, globalenv())
  for (e in parse(file.path(racine_nuee, "moteur", "serveur.R"), encoding = "UTF-8", keep.source = FALSE)) {
    texte <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
    if (startsWith(texte, "serveur <- httpuv::startServer")) break
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
proche <- function(a, b, tol = 1e-9) {
  a <- as.numeric(unlist(a))
  b <- as.numeric(unlist(b))
  length(a) == length(b) && identical(is.na(a), is.na(b)) && all(abs(a[!is.na(a)] - b[!is.na(b)]) <= tol * pmax(1, abs(b[!is.na(b)])))
}
dossier <- file.path(dirname(tempdir()), basename(tempfile("nuee_code")))
dir.create(dossier)
fichier <- function(...) file.path(dossier, ...)

# Une demande comme l'interface l'envoie : `q` passé par le JSON, la réponse mise en JSON (elle doit
# s'y écrire) ; rend la réponse telle que R l'a faite. `refus` : le message d'une demande refusée, et
# la réponse d'erreur que le serveur en fait (son corps JSON).
demande <- function(nom, q = list()) {
  q <- jsonlite::fromJSON(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA)), simplifyVector = TRUE)
  r <- DEMANDES[[nom]](q)
  repondre(200L, r)
  invisible(r)
}
refus <- function(nom, q) tryCatch({ demande(nom, q); "" }, error = conditionMessage)
reponse_erreur <- function(nom, q) {
  corps <- charToRaw(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null")))
  requete(nom, corps)
}
# Une requête HTTP comme la fenêtre ou l'application l'envoie (appli$call) : son statut et son corps.
requete <- function(nom, corps, entetes = list()) {
  req <- c(list(PATH_INFO = paste0("/api/", nom), HTTP_HOST = "127.0.0.1:8799", HTTP_COOKIE = sprintf("%s=%s", COOKIE, JETON),
                rook.input = list(read = function() corps)), entetes)
  r <- appli$call(req)
  list(statut = r$status, corps = jsonlite::fromJSON(r$body, simplifyVector = TRUE))
}
# Approuver un code comme l'application le fait : son empreinte, que le moteur calcule, ajoutée à la
# liste qu'elle garde, puis toute la liste donnée au moteur (la demande passe ici par DEMANDES, sans le
# secret : seule appli$call le demande).
approuver <- function(code) {
  e <- demande("empreinte_code", list(code = code))$empreinte
  demande("code_approuve", list(empreintes = as.list(unique(c(APPROBATIONS_CODE$empreintes, e)))))
}

# ---------------------------------------------------------------- les données

# 60 observations sur 6 sites : une mesure y d'une pente sur x, un effet aléatoire de site, un
# comptage n, un groupe qualitatif g ; deux valeurs manquantes de y.
set.seed(2)
site <- rep(sprintf("S%d", 1:6), each = 10)
x <- round(runif(60, 0, 10), 3)
g <- rep(c("témoin", "traité"), 30)
y <- round(3 + 0.6 * x + rnorm(6, 0, 1)[as.integer(factor(site))] + 0.8 * (g == "traité") + rnorm(60, 0, 0.7), 4)
y[c(7, 44)] <- NA
n <- rpois(60, exp(0.3 + 0.12 * x))
df <- data.frame(nom = sprintf("o%02d", 1:60), site = site, x = x, g = g, y = y, n = n)
source_csv <- fichier("mesures.csv")
utils::write.csv(df, source_csv, row.names = FALSE, na = "")
r <- demande("ouvrir", list(chemin = source_csv))
pid <- r$id
p <- function() projet(pid)
dd <- df[c("site", "x", "g", "y", "n")]
dd$site <- factor(dd$site)
dd$g <- factor(dd$g, levels = c("témoin", "traité"))

# Un modèle de code, sa définition comme New model l'envoie ; ajuste_code : ajusté (rend sa réponse).
def_code <- function(code, nom = "R1", ...) c(list(projet = pid, type = "CODE", nom = nom, titre = "", options = list(code = code)), list(...))
ajuste_code <- function(code, nom = "R1", ...) demande("ajuster", def_code(code, nom, ...))
e_de <- function(r) etat$modeles[[r$id]]

cat("Les données que le code voit\n")
dc <- donnees_code(p(), list(observations = numeros_observations(p())))
verifier("donnees : une colonne par variable sous son nom, les qualitatives en facteurs dans l'ordre du projet, les lignes du projet",
         identical(names(dc), c("site", "x", "g", "y", "n")) && is.factor(dc$site) && identical(levels(dc$g), c("témoin", "traité")) &&
           nrow(dc) == 60 && proche(dc$x, df$x) && sum(is.na(dc$y)) == 2 && identical(rownames(dc), as.character(1:60)))

# ---------------------------------------------------------------- la confiance

cat("La confiance : un code jamais approuvé ne s'exécute pas\n")
code_lm <- "fit <- lm(y ~ x + g, data = donnees)\nsummary(fit)$r.squared\nfit"
q_lm <- def_code(code_lm)
r_refus <- reponse_erreur("ajuster", q_lm)
verifier("un code jamais approuvé : refusé, avec son empreinte, et rien n'est ajusté (400, code_non_approuve)",
         r_refus$statut == 400 && isTRUE(r_refus$corps$code_non_approuve) && identical(r_refus$corps$empreinte, empreinte_code(code_lm)) &&
           grepl("approuvé", r_refus$corps$erreur) && !length(Filter(function(e) identical(e$definition$type, "CODE"), etat$modeles)))
r_fenetre <- requete("code_approuve", charToRaw(sprintf('{"empreintes":["%s"]}', empreinte_code(code_lm))))
verifier("une fenêtre ne l'approuve pas elle-même : sans le secret de l'application, code_approuve est refusé (403)",
         r_fenetre$statut == 403 && !code_approuve(code_lm))
r_appli <- requete("code_approuve", charToRaw(sprintf('{"empreintes":["%s"]}', empreinte_code(code_lm))), list(HTTP_X_NUEE_CODE = "secret-de-l-essai"))
verifier("l'application, avec son secret, le donne : approuvé (toute la liste remplacée)",
         r_appli$statut == 200 && code_approuve(code_lm) && identical(APPROBATIONS_CODE$empreintes, empreinte_code(code_lm)))
sha_de_r <- tryCatch(getExportedValue("tools", "sha256sum"), error = function(e) NULL)  # (R 4.5 ou plus récent)
octets_lm <- charToRaw(enc2utf8(code_lm))
verifier("l'empreinte : le SHA-256 du code en UTF-8, les fins de ligne de Windows comptant comme \\n",
         identical(empreinte_code(code_lm), if (is.null(sha_de_r)) sha256_r(octets_lm) else unname(as.character(sha_de_r(bytes = octets_lm)))) &&
           identical(empreinte_code(gsub("\n", "\r\n", code_lm)), empreinte_code(code_lm)))
verifier("… la même avec un R d'avant 4.5 (sans tools::sha256sum) : sha256_r, en R seul (les vecteurs de la FIPS 180-2)",
         identical(sha256_r(charToRaw("abc")), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad") &&
           identical(sha256_r(raw(0)), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855") &&
           identical(sha256_r(charToRaw("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")),
                     "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1") &&
           identical(sha256_r(octets_lm), empreinte_code(code_lm)))

# ---------------------------------------------------------------- lm, glm, nls, nlme

cat("Les modèles : les nombres de R\n")
m_lm <- ajuste_code(code_lm)
attendu <- lm(y ~ x + g, data = dd)
cs <- coef(summary(attendu))
co <- m_lm$coefficients
verifier("lm : les coefficients de coef(summary()), leurs erreurs-types, t, p, et l'intervalle de confint()",
         identical(co$terme, rownames(cs)) && proche(co$estimation, cs[, 1]) && proche(co$erreur_type, cs[, 2]) && proche(co$statistique, cs[, 3]) &&
           proche(co$p, cs[, 4]) && proche(cbind(co$bas, co$haut), confint(attendu)))
verifier("lm : N (sans les 2 lignes sans y), logLik et ses ddl, AIC, BIC ; la classe ; les valeurs ajustées et les résidus de ses 58 observations",
         m_lm$N == 58 && m_lm$n_donnees == 60 && proche(m_lm$statistiques$logLik, logLik(attendu)) && m_lm$statistiques$ddl == 4 &&
           proche(m_lm$statistiques$AIC, AIC(attendu)) && proche(m_lm$statistiques$BIC, BIC(attendu)) && identical(as.character(m_lm$classe), "lm") &&
           proche(m_lm$ajustees$ajuste, fitted(attendu)) && length(m_lm$ajustees$uid) == 58 &&
           !any(numeros_observations(p(), c("o07", "o44")) %in% m_lm$ajustees$uid))
verifier("ce que l'interface reçoit : une analyse qui prédit, sa ligne de l'explorateur, ses vues, le code, le résumé de summary(), la sortie du code",
         isTRUE(m_lm$analyse) && isTRUE(m_lm$code_r) && isTRUE(m_lm$prediction) && grepl("^lm · N 58 · AIC ", m_lm$chiffres) &&
           identical(as.character(m_lm$vues_defaut), c("code_coefficients", "code_diagnostics")) && identical(m_lm$code, code_lm) &&
           any(grepl("Residual standard error", m_lm$resume)) && identical(as.character(m_lm$sortie), sprintf("[1] %s", format(summary(attendu)$r.squared))) &&
           identical(m_lm$type, "CODE"))
d_lm <- Find(function(d) identical(d$name, "R1"), p()$models)
verifier("sa définition dans le projet : le code, les colonnes qu'il nomme (y, x, g ; pas n), aucun terme X ni Y",
         identical(d_lm$options$code, code_lm) && !length(d_lm$x) && !length(d_lm$y) &&
           setequal(variables_definition(d_lm), as.numeric(numeros(p(), c("y", "x", "g")))))

code_glm <- "glm(n ~ x, family = poisson, data = donnees)"
approuver(code_glm)
m_glm <- ajuste_code(code_glm, "R2")
a_glm <- glm(n ~ x, family = poisson, data = dd)
verifier("glm (Poisson) : coefficients, z, p, l'intervalle de la loi normale ; AIC",
         proche(m_glm$coefficients$estimation, coef(a_glm)) && proche(m_glm$coefficients$p, coef(summary(a_glm))[, 4]) &&
           all(is.na(m_glm$coefficients$ddl)) && proche(m_glm$coefficients$haut, coef(a_glm) + qnorm(0.975) * sqrt(diag(vcov(a_glm)))) &&
           proche(m_glm$statistiques$AIC, AIC(a_glm)) && m_glm$N == 60)

code_nls <- "nls(y ~ a + b * x, data = donnees, start = list(a = 1, b = 1))"
approuver(code_nls)
m_nls <- ajuste_code(code_nls, "R3")
a_nls <- nls(y ~ a + b * x, data = dd, start = list(a = 1, b = 1))
verifier("nls : ses deux paramètres, leurs erreurs-types et t, sa vraisemblance", identical(m_nls$coefficients$terme, c("a", "b")) &&
           proche(m_nls$coefficients$estimation, coef(a_nls), 1e-7) && proche(m_nls$coefficients$erreur_type, coef(summary(a_nls))[, 2], 1e-6) &&
           proche(m_nls$statistiques$logLik, logLik(a_nls), 1e-7))

code_lme <- "library(nlme)\nfit <- lme(y ~ x + g, random = ~ 1 | site, data = donnees, method = \"ML\", na.action = na.omit)\nfit"
approuver(code_lme)
m_lme <- ajuste_code(code_lme, "R4")
a_lme <- nlme::lme(y ~ x + g, random = ~ 1 | site, data = dd, method = "ML", na.action = na.omit)
verifier("un modèle mixte de nlme (ML) : la table de lme (Value, Std.Error, DF, t-value, p-value), l'intervalle de la loi t à ses DF, logLik, AIC ; le paquet nlme et sa version",
         proche(m_lme$coefficients$estimation, nlme::fixef(a_lme)) && proche(m_lme$coefficients$ddl, coef(summary(a_lme))[, "DF"]) &&
           proche(m_lme$coefficients$bas, nlme::fixef(a_lme) - qt(0.975, coef(summary(a_lme))[, "DF"]) * coef(summary(a_lme))[, "Std.Error"]) &&
           proche(m_lme$statistiques$AIC, AIC(a_lme)) && m_lme$N == 58 &&
           identical(m_lme$paquets$nom, "nlme") && identical(m_lme$paquets$version, utils::packageDescription("nlme")$Version))
code_reml <- "nlme::lme(y ~ x + g, random = ~ 1 | site, data = donnees, na.action = na.omit)"
approuver(code_reml)
m_reml <- ajuste_code(code_reml, "R5")
verifier("le même par REML (nlme:: sans library) : son paquet relevé, et écarté de la comparaison par l'AIC, la raison dite",
         identical(m_reml$paquets$nom, "nlme") && is.null(m_reml$comparaison$signature) && grepl("REML", m_reml$comparaison$raison))
if (requireNamespace("mgcv", quietly = TRUE)) {
  code_gam <- "library(mgcv)\ng <- gam(y ~ s(x, k = 5) + g, data = donnees)\nplot(g, pages = 1)\ng"
  approuver(code_gam)
  m_gam <- ajuste_code(code_gam, "R6")
  verifier("mgcv : ajusté, ses coefficients par coef() et vcov(), le graphique qu'il dessine, son paquet, hors de la comparaison par l'AIC",
           length(m_gam$images) == 1 && startsWith(m_gam$images[1], "data:image/png;base64,") && "mgcv" %in% m_gam$paquets$nom &&
             "s(x).1" %in% m_gam$coefficients$terme && is.null(m_gam$comparaison$signature) && grepl("gam", m_gam$comparaison$raison))
} else cat("  (mgcv absent : pas de GAM)\n")
if (requireNamespace("MASS", quietly = TRUE)) {
  code_nb <- "MASS::glm.nb(n ~ x, data = donnees)"
  approuver(code_nb)
  m_nb <- ajuste_code(code_nb, "R7")
  verifier("MASS::glm.nb : sa classe (negbin), son paquet, et la mesure discrète de sa vraisemblance pour l'AIC",
           identical(as.character(m_nb$classe)[1], "negbin") && "MASS" %in% m_nb$paquets$nom && grepl(" discrete marginale$", m_nb$comparaison$signature))
} else cat("  (MASS absent)\n")
# les colonnes de coef(summary()) reconnues par leur nom, jamais par leur place : survival::coxph
# (coef, exp(coef), se(coef), robust se…), nnet::multinom (ses coefficients en tableau, une colonne par
# terme, dont une variable z) ; les degrés de liberté de la loi t d'un gls (N − p, comme intervals())
if (requireNamespace("survival", quietly = TRUE)) {
  code_cox <- "survival::coxph(survival::Surv(n + 1, as.integer(x > 3)) ~ y + g, data = donnees)"
  approuver(code_cox)
  m_cox <- ajuste_code(code_cox, "S1")
  a_cox <- survival::coxph(survival::Surv(n + 1, as.integer(x > 3)) ~ y + g, data = dd)
  code_rob <- "survival::coxph(survival::Surv(n + 1, as.integer(x > 3)) ~ y + g, data = donnees, robust = TRUE)"
  approuver(code_rob)
  m_rob <- ajuste_code(code_rob, "S2")
  a_rob <- survival::coxph(survival::Surv(n + 1, as.integer(x > 3)) ~ y + g, data = dd, robust = TRUE)
  verifier("survival::coxph : l'erreur-type se(coef), pas exp(coef) ; z, p et l'intervalle de confint() ; avec robust = TRUE, l'erreur-type robuste, celle de son test",
           proche(m_cox$coefficients$erreur_type, coef(summary(a_cox))[, "se(coef)"]) && proche(m_cox$coefficients$statistique, coef(summary(a_cox))[, "z"]) &&
             proche(cbind(m_cox$coefficients$bas, m_cox$coefficients$haut), confint(a_cox)) &&
             proche(m_rob$coefficients$erreur_type, coef(summary(a_rob))[, "robust se"]) && proche(cbind(m_rob$coefficients$bas, m_rob$coefficients$haut), confint(a_rob)))
} else cat("  (survival absent : pas de coxph)\n")
if (requireNamespace("nnet", quietly = TRUE)) {
  code_mn <- "fit <- nnet::multinom(site ~ x + n, data = donnees, trace = FALSE)\nfit"
  approuver(code_mn)
  m_mn <- ajuste_code(code_mn, "S3")
  a_mn <- nnet::multinom(site ~ x + n, data = dd, trace = FALSE)
  b_mn <- coef(a_mn)
  s_mn <- demande("script_r", list(modele = m_mn$id, chemin = fichier("S3.R"), langue = "fr"))
  verifier("nnet::multinom : coef() et les erreurs-types de vcov(), catégorie:terme dans l'ordre de vcov(), pas son tableau de coefficients ; l'export vers R les redonne",
           identical(m_mn$coefficients$terme, rownames(vcov(a_mn))) && proche(m_mn$coefficients$estimation, c(t(b_mn))) &&
             proche(m_mn$coefficients$erreur_type, c(t(summary(a_mn)$standard.errors)), 1e-8) && all(m_mn$coefficients$bas < m_mn$coefficients$haut) &&
             all(is.na(m_mn$coefficients$statistique)) && is.null(s_mn$ecart) && any(grepl("c(t(coef(modele)))", readLines(s_mn$script, encoding = "UTF-8"), fixed = TRUE)))
} else cat("  (nnet absent : pas de multinom)\n")
code_gls <- "nlme::gls(y ~ x + g, data = donnees, na.action = na.omit)"
approuver(code_gls)
m_gls <- ajuste_code(code_gls, "S4")
a_gls <- nlme::gls(y ~ x + g, data = dd, na.action = na.omit)
verifier("nlme::gls : la loi t à N − p degrés de liberté (55), celle de sa valeur p et de intervals() (df.residual() n'en dit rien)",
         all(m_gls$coefficients$ddl == 55) && proche(m_gls$coefficients$bas, nlme::intervals(a_gls, which = "coef")$coef[, "lower"], 1e-8) &&
           proche(m_gls$coefficients$haut, nlme::intervals(a_gls, which = "coef")$coef[, "upper"], 1e-8))

# ---------------------------------------------------------------- la forme liste, la sortie, les erreurs

cat("La forme liste, ce que le code affiche, ses erreurs\n")
code_liste <- paste("fit <- lm(y ~ x, donnees)", "message(\"un message\")", "warning(\"un avertissement\")", "print(\"affiché\")", "plot(donnees$x, donnees$y)",
                    "list(modele = fit, valeurs = c(pente = coef(fit)[[\"x\"]], n_lignes = nrow(donnees)),",
                    "     tableaux = list(par_site = aggregate(y ~ site, donnees, mean), matrice = cbind(a = 1:2, b = 3:4)), note = \"autre\")", sep = "\n")
approuver(code_liste)
m_liste <- ajuste_code(code_liste, "L1")
verifier("ses valeurs (des nombres nommés), ses tableaux (data.frame et matrice), l'élément qu'il ne reconnaît pas",
         identical(as.character(m_liste$valeurs$nom), c("pente", "n_lignes")) && proche(m_liste$valeurs$valeur, c(coef(lm(y ~ x, dd))[["x"]], 60)) &&
           identical(vapply(m_liste$tableaux, `[[`, "", "nom"), c("par_site", "matrice")) && m_liste$tableaux[[1]]$n == 6 &&
           identical(as.character(m_liste$tableaux[[1]]$colonnes), c("site", "y")) && identical(as.character(m_liste$autres), "note"))
verifier("ce qu'il affiche, son message, son avertissement, le graphique qu'il dessine ; le modele de la liste ajusté",
         identical(as.character(m_liste$messages), "un message") && identical(as.character(m_liste$avertissements), "un avertissement") &&
           any(grepl("affiché", m_liste$sortie)) && length(m_liste$images) == 1 && identical(m_liste$coefficients$terme, c("(Intercept)", "x")))
seules <- "list(valeurs = c(a = 1), tableaux = list(t = data.frame(u = 1:3)))"
approuver(seules)
m_seules <- ajuste_code(seules, "L2")
verifier("une liste sans modele : ses valeurs et ses tableaux seulement (ni coefficients, ni résumé)",
         is.null(m_seules$coefficients) && !length(m_seules$resume) && identical(as.character(m_seules$valeurs$nom), "a") && length(m_seules$tableaux) == 1)

erreur <- function(code) {
  approuver(code)
  r <- reponse_erreur("ajuster", def_code(code, "E1"))
  c(statut = r$statut, erreur = r$corps$erreur %||% "", ligne = r$corps$ligne %||% NA)
}
e1 <- erreur("x <- 1\nfit <- lm(y ~ z, data = donnees)\nfit")
verifier(sprintf("une erreur : le message de R et sa ligne (« %s »)", e1[["erreur"]]),
         e1[["statut"]] == "400" && grepl("ligne 2", e1[["erreur"]]) && grepl("'z'", e1[["erreur"]]) && e1[["ligne"]] == "2" &&
           is.null(Find(function(d) identical(d$name, "E1"), p()$models)))
e2 <- erreur("fit <- lm(y ~ x, data = donnees\nfit")
verifier("une erreur de lecture : le message du lecteur de R, sa ligne", grepl("unexpected", e2[["erreur"]]) && e2[["ligne"]] == "2")
e3 <- erreur("library(paquetQuiNExistePas)\n1")
verifier(sprintf("un paquet qui manque : le message de R, comment l'installer (« %s »)", substr(e3[["erreur"]], 1, 90)),
         grepl("paquetQuiNExistePas", e3[["erreur"]]) && grepl("install.packages(\"paquetQuiNExistePas\")", e3[["erreur"]], fixed = TRUE) && e3[["ligne"]] == "1")
verifier("une valeur NULL, un code vide : refusés, en mots",
         grepl("NULL", erreur("x <- 1\nNULL")[["erreur"]]) && grepl("Écrivez le code R", refus("ajuster", def_code("  \n"))))
verifier("de mauvaises valeurs : refusées, en mots", grepl("nom", erreur("list(modele = lm(y ~ x, donnees), valeurs = c(1, 2))")[["erreur"]]))

# ---------------------------------------------------------------- l'interface, les textes, les exports

cat("Ce que l'interface reçoit, les textes, les exports, les prédictions\n")
json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA, force = TRUE)
r_liste <- demande("modele", list(modele = m_liste$id))
verifier("en JSON : coefficients, statistiques, valeurs ajustées, droite de Henry, valeurs et tableaux, images",
         inherits(json(r_liste), "json") && length(r_liste$qq$theorique) == 58 && is.numeric(r_liste$qq$pente) && length(r_liste$images) == 1)
for (langue in c("fr", "en")) {
  tm <- demande("methodes", list(modele = m_lme$id, langue = langue))
  fr <- langue == "fr"
  verifier(sprintf("texte des méthodes (%s) : le code des auteurs, les observations, la classe, AIC, R et nlme avec sa version et sa citation, les références de R, de Nuée et de nlme", langue),
           grepl(if (fr) "code R de ses auteurs" else "own R code", tm$paragraphes[1]) && grepl("58", tm$paragraphes[1]) && grepl("lme", tm$paragraphes[1]) &&
             grepl("AIC", tm$paragraphes[1]) && grepl(sprintf("nlme %s (Pinheiro", utils::packageDescription("nlme")$Version), tm$paragraphes[2], fixed = TRUE) &&
             any(grepl("R Core Team", tm$references)) && any(grepl("Boutin", tm$references)) && any(grepl("^Pinheiro", tm$references)) &&
             !any(grepl("_|<https", tm$references)))
}
tr <- tables_resultats(p(), e_de(m_liste)$modele)
verifier("tableaux exportés : résumé, coefficients, valeurs, les tableaux du code, ajustées, texte, code ; écrits en Excel",
         all(vapply(tr, is.data.frame, TRUE)) && all(c("resume", "coefficients", "valeurs", "par_site", "matrice", "ajustees", "texte", "code") %in% names(tr)) &&
           file.exists(ecrire_tables(tr, fichier("code.xlsx"), objet = "code")))
pr <- demande("predire", list(modele = m_lm$id))
verifier("les prédictions de predict(fit, newdata) sur les observations du projet ; la réponse observée",
         proche(pr$observations[["y prédit"]], predict(attendu, newdata = dd)) && proche(pr$observations[["y observé"]], dd$y))
verifier("un modèle dont predict() ne marche pas (une liste sans modele) : sans prédiction", identical(demande("modele", list(modele = m_seules$id))$prediction, FALSE))
pr_glm <- demande("predire", list(modele = m_glm$id))
verifier("les prédictions d'un glm de Poisson : sur l'échelle de la réponse, des comptages (predict(type = \"response\")), pas leur log",
         proche(pr_glm$observations[["n prédit"]], predict(a_glm, newdata = dd, type = "response")) && proche(pr_glm$observations[["n observé"]], dd$n))
cles <- vapply(demande("resultats_cles", list(modele = m_lm$id))$entrees, `[[`, "", "cle")
verifier("ses résultats clés : N, K, logLik, AIC, BIC, chaque coefficient (coef., se.) ; les valeurs d'un code (valeurs.)",
         all(c("N", "K", "logLik", "AIC", "BIC", "coef.(Intercept)", "coef.x", "coef.gtraité", "se.x") %in% cles) &&
           all(c("valeurs.pente", "valeurs.n_lignes") %in% vapply(demande("resultats_cles", list(modele = m_liste$id))$entrees, `[[`, "", "cle")))

# la comparaison par l'AIC : le même ensemble que la régression de Nuée
r_lm_nuee <- demande("ajuster", list(projet = pid, type = "LM", x = as.numeric(numeros(p(), c("x", "g"))), y = as.numeric(numeros(p(), "y")), echelle = "none", nom = "LM1"))
verifier("comparé par l'AIC à la régression linéaire de Nuée de la même réponse sur les mêmes observations : le même ensemble, le même AIC",
         identical(m_lm$comparaison$signature, r_lm_nuee$signature) && proche(m_lm$comparaison$AIC, r_lm_nuee$qualite$AIC, 1e-8))

# ---------------------------------------------------------------- l'export vers R

cat("L'export vers R : lancé seul, les mêmes nombres\n")
rscript <- file.path(R.home("bin"), "Rscript")
lanceur <- fichier("lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".m <- get(.a[3])", ".r <- list(coef = coef(summary(.m)), aic = AIC(.m), loglik = as.numeric(logLik(.m)), n = nobs(.m))",
             "if (exists(.a[4])) .r$valeurs <- get(.a[4])", "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, modele, valeurs) {
  sortie <- tempfile("sortie", dossier, ".rds")
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), modele, valeurs), stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
for (cas in list(list(r = m_lme, langue = "fr", modele = "modele", valeurs = "valeurs", donnees = "script"),
                 list(r = m_liste, langue = "en", modele = "model", valeurs = "values", donnees = "csv"))) {
  s <- demande("script_r", list(modele = cas$r$id, chemin = fichier(sprintf("%s_%s.R", cas$r$nom, cas$langue)), langue = cas$langue, donnees = cas$donnees))
  texte <- readLines(s$script, encoding = "UTF-8")
  o <- lancer(s$script, cas$modele, cas$valeurs)
  m <- e_de(cas$r)$modele
  code <- strsplit(m$code, "\n", fixed = TRUE)[[1]]
  verifier(sprintf("%s (%s, données %s) : écrit sans écart ; son code tel qu'écrit ; lancé seul par Rscript, les mêmes coefficients, AIC, logLik, N%s", cas$r$nom, cas$langue, cas$donnees,
                   if (length(m$valeurs)) " et valeurs" else ""),
           is.null(s$ecart) && all(vapply(code, function(l) any(endsWith(texte, l)), TRUE)) && length(o) > 0 && proche(o$coef[, 1], m$coefficients$estimation, 1e-10) &&
             proche(o$coef[, 2], m$coefficients$erreur_type, 1e-10) && proche(o$aic, m$statistiques$AIC, 1e-10) && proche(o$loglik, m$statistiques$logLik, 1e-10) &&
             o$n == m$N && (!length(m$valeurs) || proche(o$valeurs, m$valeurs, 1e-10)))
  if (!length(o)) cat("    ", utils::tail(attr(o, "messages"), 5), sep = "\n    ")
}
verifier("la dernière valeur nommée dans le script : resultat <- fit après le code, ou devant sa dernière expression",
         identical(utils::tail(code_resultat_script("a <- 1\nfit", "resultat"), 1), "resultat <- fit") &&
           identical(utils::tail(code_resultat_script("a <- 1; list(modele = a,\n  valeurs = c(x = 1))", "result"), 2), c("a <- 1; result <- list(modele = a,", "  valeurs = c(x = 1))")))
# (le code de l'export s'exécute dans le moteur avant d'être écrit : il doit être approuvé)
APPROBATIONS_CODE$empreintes <- setdiff(APPROBATIONS_CODE$empreintes, empreinte_code(code_lme))
verifier("l'export vers R d'un code qui n'est plus approuvé : refusé, rien n'est écrit",
         grepl("approuvé", refus("script_r", list(modele = m_lme$id, chemin = fichier("refuse.R")))) && !file.exists(fichier("refuse.R")))
approuver(code_lme)

# ---------------------------------------------------------------- Ce qui a changé, la mise à jour

cat("Les données mises à jour : le modèle réajusté, ce qui a changé\n")
avant <- demande("resultats_cles", list(modele = m_lm$id))$entrees
df2 <- rbind(df, data.frame(nom = sprintf("o%02d", 61:72), site = rep(sprintf("S%d", 1:6), 2), x = round(runif(12, 0, 10), 3), g = rep(c("témoin", "traité"), 6),
                            y = round(9 + 0.6 * runif(12, 0, 10), 4), n = rpois(12, 3)))
suivant <- fichier("mesures 2.csv")
utils::write.csv(df2, suivant, row.names = FALSE, na = "")
a <- demande("mettre_a_jour", list(projet = pid, chemin = suivant))
m_lm2 <- demande("ajuster", list(projet = pid, numero = Find(function(d) identical(d$name, "R1"), p()$models)$number))
apres <- demande("resultats_cles", list(modele = m_lm2$id))$entrees
cmp <- demande("comparer_resultats", list(avant = list(entrees = avant), apres = list(entrees = apres), langue = "fr"))
ent <- function(cle) Find(function(x) identical(x$cle, cle), cmp$entrees)
verifier("réajusté sur les données mises à jour : les 12 nouvelles observations y entrent (N 70) ; Ce qui a changé compare N, AIC et chaque coefficient, en phrases",
         a$observations$ajoutees == 12 && m_lm2$N == 70 && ent("N")$avant == 58 && ent("N")$apres == 70 && !is.null(ent("coef.x")) &&
           !is.null(ent("AIC")$ecart) && length(cmp$phrases) > 0)
# une colonne que le code nomme disparaît du fichier : la mise à jour le dit, comme pour les autres modèles
manque <- fichier("mesures sans g.csv")
utils::write.csv(df2[setdiff(names(df2), "g")], manque, row.names = FALSE, na = "")
a3 <- demande("mettre_a_jour", list(projet = pid, chemin = manque, apercu = TRUE))
touches <- a3$modeles$variables_absentes
r1 <- Find(function(x) identical(x$nom, "R1"), touches)
verifier("une colonne que le code nomme absente du fichier : la mise à jour nomme le modèle et la variable ; un modèle dont le code ne la nomme pas, non",
         !is.null(r1) && identical(as.character(r1$variables), "g") && is.null(Find(function(x) identical(x$nom, "R2"), touches)))

# ---------------------------------------------------------------- un projet venu d'ailleurs

cat("Un projet enregistré, puis son code changé ailleurs\n")
chemin <- fichier("projet.nuee")
demande("enregistrer", list(projet = pid, chemin = chemin))
j <- jsonlite::fromJSON(chemin, simplifyVector = FALSE)
verifier("le fichier : le format de la 1.1 (3), le code et les colonnes du modèle", identical(j$nuee$format, 3L) &&
           any(vapply(j$models, function(d) identical(d$type, "CODE") && identical(d$options$code, code_lm), TRUE)))
autre <- "fit <- lm(y ~ x + g, data = donnees)\nfit  # (changé par un collègue)"
for (k in seq_along(j$models)) if (identical(j$models[[k]]$name, "R1")) j$models[[k]]$options$code <- autre
copie <- fichier("copie.nuee")
writeLines(jsonlite::toJSON(j, auto_unbox = TRUE, null = "null", digits = NA), copie, useBytes = TRUE)
r2 <- demande("ouvrir", list(chemin = copie))
s1 <- Find(function(d) identical(d$nom, "R1"), r2$modeles)
s2 <- Find(function(d) identical(d$nom, "R2"), r2$modeles)
verifier("rouvert : son résumé dit le code de R1 non approuvé (son empreinte), celui de R2, le même qu'ici, approuvé",
         isFALSE(s1$code_r$approuve) && identical(s1$code_r$empreinte, empreinte_code(autre)) && isTRUE(s2$code_r$approuve))
rr <- reponse_erreur("ajuster", list(projet = r2$id, numero = s1$numero))
verifier("l'ajuster, comme le fait la session qui se restaure : refusé, rien ne s'exécute",
         rr$statut == 400 && isTRUE(rr$corps$code_non_approuve) && !any(vapply(etat$modeles, function(e) identical(e$projet, r2$id), TRUE)))
cm <- demande("code_modeles", list(projet = r2$id))$modeles
verifier("Montrer le code : chaque modèle de code R du projet, son code tel qu'il est, s'il est approuvé",
         identical(Find(function(x) identical(x$nom, "R1"), cm)$code, autre) && isFALSE(Find(function(x) identical(x$nom, "R1"), cm)$approuve))
approuver(autre)
ok <- demande("ajuster", list(projet = r2$id, numero = s1$numero))
verifier("Exécuter : approuvé, il s'ajuste", identical(ok$type, "CODE") && identical(ok$code, autre))

# ---------------------------------------------------------------- la politique

cat("La politique qui retire le type (DisableCustomCode)\n")
Sys.setenv(NUEE_SANS_CODE = "1")
verifier("le type n'est pas proposé (etat) et le moteur le refuse : une définition nouvelle, un modèle déjà approuvé, l'empreinte",
         isTRUE(etat_complet()$code_interdit) && grepl("DisableCustomCode", refus("ajuster", def_code(code_lm, "P1"))) &&
           grepl("DisableCustomCode", refus("ajuster", list(projet = r2$id, numero = s1$numero))) && grepl("DisableCustomCode", refus("empreinte_code", list(code = "1"))))
Sys.unsetenv("NUEE_SANS_CODE")
verifier("sans elle, de nouveau permis", !isTRUE(etat_complet()$code_interdit) && identical(demande("ajuster", def_code(code_lm, "P1"))$type, "CODE"))

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
