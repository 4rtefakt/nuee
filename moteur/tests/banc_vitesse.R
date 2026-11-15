# Banc d'essai de la vitesse du moteur. Ce n'est pas une vérification (l'intégration continue ne
# le lance pas) : il affiche des temps, et peut garder les résultats de chaque cas pour comparer
# deux versions du moteur.
#
# Des données simulées, réalistes, à trois tailles (petit, moyen, grand) :
#   - spectres proche infrarouge (les bandes de quatre constituants, une diffusion multiplicative,
#     un décalage, une pente et du bruit) de 100 × 500, 500 × 2 000 et 2 000 × 2 000 : ouverts en
#     CSV ; ACP, PLS, PLS-DA et OPLS à nombre de composantes automatique (validation croisée en
#     7 groupes), après SNV et dérivée première de Savitzky–Golay (fenêtre 15, degré 2) ; le test
#     de permutation (100 permutations) et la CV-ANOVA d'une PLS ; DModX, T² et contributions ;
#     le JSON des vues d'un modèle (le modèle, ses spectres, le biplot, les données), taille et
#     temps ;
#   - relevés écologiques (sites, années, habitats, température, profondeur) de 2 000, 5 000 et
#     20 000 lignes : un modèle linéaire mixte et un GAM, l'un et l'autre à effet aléatoire du
#     site et validés par site ;
#   - des tableaux de 1 000, 10 000 et 100 000 lignes ouverts en CSV et en .xlsx, et leurs données
#     envoyées à la vue Données ;
#   - un projet de spectres (petit, moyen) à 10 modèles ajustés, enregistré puis rouvert, ses
#     modèles refaits comme quand l'interface restaure la séance.
# Chaque cas passe par les demandes du serveur (serveur.R : DEMANDES, journaliser, repondre), telles
# que l'interface les envoie (en JSON, relues par jsonlite), et se refait --repetitions fois (3 par
# défaut) depuis le même état : le temps donné est la médiane. Ses résultats (les réponses, le
# modèle ajusté, le JSON envoyé) peuvent être gardés (--garder) puis comparés à ceux d'une autre
# version du moteur (--comparer) : identical(), sinon all.equal(tolerance = 1e-12), cas par cas.
#
#   Rscript moteur/tests/banc_vitesse.R [--moteur=<racine d'une copie de Nuée, celle-ci par défaut>]
#       [--tailles=petit,moyen,grand] [--cas=<expression régulière sur le nom des cas>]
#       [--repetitions=3] [--garder=<dossier>] [--comparer=<dossier>] [--profil=<dossier>]
#       [--temps=<fichier .csv des temps>] [--travail=<dossier des fichiers ouverts>]
# Avec --profil, le premier passage de chaque cas est suivi par Rprof (<cas>.out, et son résumé
# <cas>.txt). Les temps varient d'un ordinateur à l'autre, et avec ce qui y tourne en même temps :
# comparer deux versions dans les mêmes conditions.

options(warn = 1, stringsAsFactors = FALSE)
banc_arguments <- commandArgs(trailingOnly = TRUE)
banc_option <- function(nom, defaut = NULL) {
  a <- grep(paste0("^--", nom, "="), banc_arguments, value = TRUE)
  if (length(a)) sub(paste0("^--", nom, "="), "", a[length(a)]) else defaut
}
banc_racine <- normalizePath(banc_option("moteur", "."), winslash = "/")
banc_tailles <- strsplit(banc_option("tailles", "petit,moyen,grand"), ",", fixed = TRUE)[[1]]
banc_motif <- banc_option("cas", "")
banc_repetitions <- as.integer(banc_option("repetitions", "3"))
banc_garder <- banc_option("garder")
banc_comparer <- banc_option("comparer")
banc_profil <- banc_option("profil")
banc_fichier_temps <- banc_option("temps")
# (les fichiers que les cas ouvrent : un dossier --travail les garde d'un passage à l'autre)
banc_dossier <- banc_option("travail", file.path(tempdir(), "banc"))
dir.create(banc_dossier, showWarnings = FALSE, recursive = TRUE)
for (d in c(banc_garder, banc_profil)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Le serveur sans son écoute : chaque expression de serveur.R jusqu'au démarrage de httpuv, sa
# racine et ses arguments donnés ici (DEMANDES, journaliser, repondre et l'état `etat` en viennent).
banc_charger <- function(racine_nuee) {
  assign("racine", racine_nuee, globalenv())
  exprs <- parse(file.path(racine_nuee, "moteur", "serveur.R"), encoding = "UTF-8", keep.source = FALSE)
  for (e in exprs) {
    texte <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
    if (startsWith(texte, "serveur <- httpuv::startServer")) break
    if (startsWith(texte, "local(") && grepl("racine <<-", texte, fixed = TRUE)) next
    if (startsWith(texte, "args <- commandArgs")) {
      assign("args", c("8799", file.path(racine_nuee, "app", "interface")), globalenv())
      next
    }
    eval(e, globalenv())
  }
}
banc_charger(banc_racine)
options(nuee.langue = "fr")

banc_horloge <- function() proc.time()[["elapsed"]]

# ---------------------------------------------------------------- données simulées

# Des spectres proche infrarouge : N spectres de K longueurs d'onde de 1100 à 2498 nm, mélanges de
# quatre constituants (trois bandes gaussiennes chacun au plus), avec une diffusion multiplicative,
# un décalage, une pente et du bruit ; la protéine (le premier constituant) et une classe de la
# teneur du deuxième en trois niveaux.
banc_spectres <- function(N, K, graine = 1) {
  set.seed(graine)
  onde <- seq(1100, 2498, length.out = K)
  centres <- list(c(1200, 1450, 1940), c(1730, 2310), c(1510, 2050, 2180), c(1650, 2270))
  bandes <- vapply(centres, function(cs) rowSums(vapply(cs, function(c) exp(-0.5 * ((onde - c) / 45)^2), numeric(K))), numeric(K))
  conc <- matrix(stats::runif(N * 4), N, 4)
  conc <- conc / rowSums(conc)
  X <- stats::rnorm(N, 0, 0.1) + stats::rlnorm(N, 0, 0.15) * (conc %*% t(bandes)) +
    outer(stats::rnorm(N, 0, 0.05), (onde - 1100) / 1400) + matrix(stats::rnorm(N * K, 0, 0.002), N, K)
  y <- 10 * conc[, 1] + stats::rnorm(N, 0, 0.1)
  classe <- c("faible", "moyen", "fort")[findInterval(conc[, 2], stats::quantile(conc[, 2], c(1 / 3, 2 / 3))) + 1]
  df <- data.frame(echantillon = sprintf("E%04d", seq_len(N)), classe = classe, proteine = round(y, 3))
  spectres <- as.data.frame(round(X, 5))
  names(spectres) <- sprintf("%.1f", onde)
  cbind(df, spectres)
}

# Des relevés : n lignes réparties sur `sites` sites (chacun son effet), de 2000 à 2023, trois
# habitats, une température qui dépend du site, une profondeur ; la densité d'une espèce.
banc_releves <- function(n, sites, graine = 2) {
  set.seed(graine)
  noms <- sprintf("S%02d", seq_len(sites))
  site <- noms[(seq_len(n) - 1) %% sites + 1]
  effet <- stats::setNames(stats::rnorm(sites, 0, 0.6), noms)
  chaleur <- stats::setNames(stats::rnorm(sites, 0, 1.5), noms)
  annee <- sample(2000:2023, n, replace = TRUE)
  habitat <- sample(c("herbier", "sable", "roche"), n, replace = TRUE)
  temperature <- round(15 + chaleur[site] + stats::rnorm(n, 0, 2.5), 2)
  profondeur <- round(stats::runif(n, 1, 30), 1)
  densite <- 2 + 0.25 * temperature - 0.04 * profondeur + c(herbier = 0.5, sable = 0, roche = -0.3)[habitat] +
    0.6 * sin((temperature - 12) / 3) + effet[site] + 0.02 * (annee - 2000) + stats::rnorm(n, 0, 0.8)
  data.frame(releve = sprintf("R%06d", seq_len(n)), site = site, annee = annee, habitat = habitat,
             temperature = unname(temperature), profondeur = profondeur, densite = round(unname(densite), 3))
}

# Un tableau de suivi : des identifiants, des variables qualitatives, des mesures (quelques valeurs
# manquantes et des valeurs sous la limite de détection écrites « <0,5 »), des comptages.
banc_tableau <- function(n, graine = 3) {
  set.seed(graine)
  df <- data.frame(prelevement = sprintf("P%06d", seq_len(n)),
                   site = sprintf("S%03d", sample(1:150, n, replace = TRUE)),
                   station = sprintf("St%d", sample(1:6, n, replace = TRUE)),
                   saison = sample(c("printemps", "été", "automne", "hiver"), n, replace = TRUE),
                   annee = sample(1995:2024, n, replace = TRUE),
                   temperature = round(stats::rnorm(n, 14, 5), 2), salinite = round(stats::runif(n, 0, 38), 2),
                   oxygene = round(stats::rnorm(n, 8, 1.5), 2), ph = round(stats::rnorm(n, 7.9, 0.3), 2),
                   turbidite = round(stats::rlnorm(n, 1, 0.8), 3), chlorophylle = round(stats::rlnorm(n, 0.5, 1), 3))
  no3 <- format(round(stats::rlnorm(n, 0, 1), 3))
  no3[sample(n, n %/% 50)] <- "<0,5"
  df$nitrates <- trimws(no3)
  for (k in c("temperature", "oxygene")) df[[k]][sample(n, n %/% 40)] <- NA
  for (e in sprintf("espece_%02d", 1:9)) df[[e]] <- stats::rpois(n, stats::rgamma(1, 2, 0.5))
  df
}

TAILLES_SPECTRES <- list(petit = c(100, 500), moyen = c(500, 2000), grand = c(2000, 2000))
TAILLES_RELEVES <- list(petit = c(2000, 20), moyen = c(5000, 25), grand = c(20000, 40))
TAILLES_TABLEAUX <- list(petit = 1000, moyen = 10000, grand = 100000)

# ---------------------------------------------------------------- demandes au serveur

# Une demande comme l'interface l'envoie : `q` passé par le JSON (ses listes en data.frames, ses
# nombres seuls en vecteurs), la réponse calculée, journalisée pour la console R et mise en JSON.
banc_demande <- function(nom, q) {
  q <- jsonlite::fromJSON(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA)), simplifyVector = TRUE)
  t0 <- banc_horloge()
  r <- DEMANDES[[nom]](q)
  t1 <- banc_horloge()
  tryCatch(journaliser(nom, q, r), error = function(e) NULL)
  t2 <- banc_horloge()
  corps <- repondre(200L, r)$body
  t3 <- banc_horloge()
  list(r = r, json = corps, calcul = t1 - t0, journal = t2 - t1, temps_json = t3 - t2, octets = nchar(corps, "bytes"))
}

# Un projet fait d'un data.frame, ouvert comme un fichier (serveur.R : ouvrir_fichier).
banc_projet <- function(df, nom) {
  p <- projet_depuis_tableau(df, nom)
  p$id <- nouvel_id("P")
  p$nom <- nom
  p$chemin <- file.path(banc_dossier, nom)
  etat$projets[[p$id]] <- p
  p$id
}

# La demande d'ajustement de la boîte Nouveau modèle (dialogues.js), pour des spectres : les
# longueurs d'onde en X (variables 3 à K + 2), la protéine en Y (variable 2) ou la classe (1).
banc_q_spectres <- function(projet, type, K, A = 0, etapes = list(list(type = "snv"), list(type = "sg", derivee = 1, fenetre = 15, degre = 2)), nom = "M1") {
  vide <- stats::setNames(list(), character(0))
  list(projet = projet, type = type, x = 3:(K + 2), y = if (type %in% c("PLS", "OPLS")) 2 else list(),
       classe = if (type %in% c("PLS-DA", "OPLS-DA")) 1 else NULL, exclues = list(), test = list(), echelle = "UV",
       echelles = vide, transformations = vide, croises = list(), spectres = etapes, A = A, G = 7, vc = NULL,
       blocs = vide, modificateurs = vide, aleatoires = list(), correlation = NULL, famille = NULL, lisses = list(),
       k_courbes = vide, par_courbes = vide, validation = "centres", decalage = NULL, lisse_spatial = NULL,
       croises_reglages = vide, nom = nom, titre = "", condition = "")
}

# Une régression des relevés (variables : 1 site, 2 annee, 3 habitat, 4 temperature, 5 profondeur,
# 6 densite) : densité ~ température + profondeur + habitat + année, effet aléatoire du site (la
# validation croisée est alors par site) ; un GAM, la température en courbe.
banc_q_releves <- function(projet, type) {
  vide <- stats::setNames(list(), character(0))
  list(projet = projet, type = type, x = c(4, 5, 3, 2), y = 6, classe = NULL, exclues = list(), test = list(), echelle = "UV",
       echelles = vide, transformations = vide, croises = list(), spectres = list(), A = 0, G = 7, vc = NULL,
       blocs = vide, modificateurs = vide, aleatoires = list(list(by = "variable", variable = 1)), correlation = NULL,
       famille = if (type == "GAM") "gaussienne" else NULL, lisses = if (type == "GAM") 4 else list(),
       k_courbes = vide, par_courbes = vide, validation = NULL, decalage = NULL, lisse_spatial = NULL,
       croises_reglages = vide, nom = "M1", titre = "", condition = "")
}

# Vérifier puis ajuster, comme Nouveau modèle › Ajuster : la réponse, le modèle et sa définition.
banc_ajuster <- function(q) {
  v <- banc_demande("verifier", q)
  a <- banc_demande("ajuster", q)
  e <- etat$modeles[[a$r$id]]
  list(temps = c(verifier = v$calcul, ajuster = a$calcul, journal = a$journal, json = a$temps_json),
       octets = a$octets, sorties = list(verification = v$r, reponse = a$r, json = a$json, modele = banc_sans_ajustement(e$modele), definition = e$definition))
}

# Un projet, ou ce qu'en dit resume_projet, sans ce qui tient à la séance ou au dossier de travail
# (son identifiant, ses chemins, l'état de son fichier) : deux versions se comparent par le reste.
banc_sans_chemins <- function(p) p[setdiff(names(p), c("id", "chemin", "chemin_enregistrement", "fichier_lu"))]

# Un modèle sans les ajustements de nlme et mgcv qu'il garde (des objets faits d'environnements, que
# identical() ne compare pas d'une séance à l'autre) : la régression se compare par tout le reste.
banc_sans_ajustement <- function(m) {
  if (inherits(m, "modele_regression")) m[c("ajustement", "ajustement_ml")] <- NULL
  m
}

# ---------------------------------------------------------------- les cas

banc_cas <- list()
banc_ajouter <- function(nom, preparer, executer) banc_cas[[length(banc_cas) + 1]] <<- list(nom = nom, preparer = preparer, executer = executer)

# (chaque local() garde sa taille : les fonctions des cas, appelées après les boucles, la lisent là)
for (taille in names(TAILLES_SPECTRES)) local({
  taille <- taille
  N <- TAILLES_SPECTRES[[taille]][1]
  K <- TAILLES_SPECTRES[[taille]][2]
  base <- sprintf("spectres_%s", taille)
  donnees <- function() banc_spectres(N, K)
  banc_ajouter(paste0(base, "_ouvrir_csv"), function() {
    f <- file.path(banc_dossier, sprintf("spectres_%s.csv", taille))
    if (!file.exists(f)) utils::write.csv(donnees(), f, row.names = FALSE)
    list(chemin = f)
  }, function(ctx) {
    o <- banc_demande("ouvrir", list(chemin = ctx$chemin))
    list(temps = c(ouvrir = o$calcul, json = o$temps_json), octets = o$octets,
         sorties = list(reponse = banc_sans_chemins(o$r), projet = banc_sans_chemins(etat$projets[[o$r$id]])))
  })
  for (type in c("PCA", "PLS", "PLS-DA", "OPLS")) local({
    type <- type
    banc_ajouter(sprintf("%s_%s_auto", base, tolower(sub("-", "", type))), function() list(projet = banc_projet(donnees(), sprintf("spectres_%s.csv", taille))),
                 function(ctx) banc_ajuster(banc_q_spectres(ctx$projet, type, K)))
  })
  # (une PLS de trois composantes, ajustée avant le cas)
  avec_pls <- function() {
    p <- banc_projet(donnees(), sprintf("spectres_%s.csv", taille))
    r <- banc_demande("ajuster", banc_q_spectres(p, "PLS", K, A = 3))
    list(projet = p, modele = r$r$id)
  }
  # (pas pour 2 000 spectres : cent ajustements de plusieurs secondes chacun, trois fois)
  if (taille != "grand") banc_ajouter(paste0(base, "_pls_permutations"), avec_pls, function(ctx) {
    o <- banc_demande("permutations", list(modele = ctx$modele, n = 100))
    list(temps = c(permutations = o$calcul, json = o$temps_json), octets = o$octets, sorties = list(reponse = o$r, json = o$json))
  })
  banc_ajouter(paste0(base, "_pls_diagnostics"), avec_pls, function(ctx) {
    m <- etat$modeles[[ctx$modele]]$modele
    t0 <- banc_horloge()
    s <- statistiques(m)  # (DModX, T², VIP, coefficients et leurs intervalles, CV-ANOVA)
    t1 <- banc_horloge()
    p <- banc_demande("predire", list(modele = ctx$modele))
    obs <- m$observations
    c1 <- banc_demande("contributions", list(modele = ctx$modele, observations = obs[1], type = "DModX", composante = 1, ponderee = TRUE))
    c2 <- banc_demande("contributions", list(modele = ctx$modele, observations = obs[seq(1, length(obs), 3)], type = "score", composante = 1, ponderee = TRUE))
    c3 <- banc_demande("contributions", list(modele = ctx$modele, observations = obs[2], type = "T2", composante = 1, ponderee = TRUE))
    list(temps = c(statistiques = t1 - t0, predire = p$calcul + p$temps_json, contributions = c1$calcul + c2$calcul + c3$calcul +
                     c1$temps_json + c2$temps_json + c3$temps_json, cv_anova = system.time(cv_anova(m, m$vc$predictions[[m$A]]))[["elapsed"]]),
         octets = p$octets, sorties = list(statistiques = s, prediction = p$r, json_prediction = p$json, contributions = list(c1$r, c2$r, c3$r)))
  })
  banc_ajouter(paste0(base, "_vues"), avec_pls, function(ctx) {
    vues <- list(modele = list(modele = ctx$modele), spectres = list(modele = ctx$modele), biplot = list(modele = ctx$modele), donnees = list(projet = ctx$projet))
    o <- lapply(names(vues), function(v) banc_demande(v, vues[[v]]))
    names(o) <- names(vues)
    list(temps = unlist(lapply(o, function(x) x$calcul + x$temps_json)), octets = sum(vapply(o, `[[`, 0, "octets")),
         tailles_json = vapply(o, `[[`, 0, "octets"), sorties = lapply(o, `[`, c("r", "json")))
  })
})

for (taille in names(TAILLES_RELEVES)) local({
  taille <- taille
  n <- TAILLES_RELEVES[[taille]][1]
  sites <- TAILLES_RELEVES[[taille]][2]
  for (type in c("LMM", "GAM")) local({
    type <- type
    banc_ajouter(sprintf("releves_%s_%s", taille, tolower(type)), function() list(projet = banc_projet(banc_releves(n, sites), sprintf("releves_%s.csv", taille))),
                 function(ctx) banc_ajuster(banc_q_releves(ctx$projet, type)))
  })
})

for (taille in names(TAILLES_TABLEAUX)) local({
  taille <- taille
  n <- TAILLES_TABLEAUX[[taille]]
  for (ext in c("csv", "xlsx")) local({
    ext <- ext
    banc_ajouter(sprintf("tableau_%s_%s", taille, ext), function() {
      f <- file.path(banc_dossier, sprintf("tableau_%s.%s", taille, ext))
      if (!file.exists(f)) {
        df <- banc_tableau(n)
        if (ext == "csv") utils::write.csv(df, f, row.names = FALSE, na = "", fileEncoding = "UTF-8") else openxlsx::write.xlsx(df, f)
      }
      list(chemin = f)
    }, function(ctx) {
      o <- banc_demande("ouvrir", list(chemin = ctx$chemin))
      d <- banc_demande("donnees", list(projet = o$r$id))
      list(temps = c(ouvrir = o$calcul, json = o$temps_json, donnees = d$calcul + d$temps_json), octets = o$octets + d$octets,
           sorties = list(reponse = banc_sans_chemins(o$r), projet = banc_sans_chemins(etat$projets[[o$r$id]]), donnees = d$json))
    })
  })
})

# Un projet de spectres à 10 modèles ajustés : enregistré avec la séance qui les nomme, puis rouvert
# et ses modèles refaits (app.js : restaurerSession, un ajustement par modèle, A donné).
for (taille in c("petit", "moyen")) local({
  taille <- taille
  N <- TAILLES_SPECTRES[[taille]][1]
  K <- TAILLES_SPECTRES[[taille]][2]
  banc_ajouter(sprintf("projet_%s_10_modeles", taille), function() {
    p <- banc_projet(banc_spectres(N, K), sprintf("spectres_%s.csv", taille))
    snv <- list(list(type = "snv"))
    sg <- list(list(type = "snv"), list(type = "sg", derivee = 1, fenetre = 15, degre = 2))
    msc <- list(list(type = "msc"), list(type = "sg", derivee = 2, fenetre = 21, degre = 3))
    defs <- list(list("PCA", 3, list()), list("PCA", 4, sg), list("PLS", 3, list()), list("PLS", 4, snv), list("PLS", 4, sg),
                 list("PLS", 5, msc), list("PLS-DA", 3, sg), list("PLS-DA", 4, snv), list("OPLS", 3, sg), list("OPLS", 2, list(list(type = "detrend", degre = 2))))
    modeles <- lapply(seq_along(defs), function(i) {
      r <- banc_demande("ajuster", banc_q_spectres(p, defs[[i]][[1]], K, A = defs[[i]][[2]], etapes = defs[[i]][[3]], nom = paste0("M", i)))$r
      list(numero = r$numero, A = r$A)
    })
    list(projet = p, session = as.character(jsonlite::toJSON(list(version = 2, modeles = modeles, vues = list()), auto_unbox = TRUE)),
         chemin = file.path(banc_dossier, sprintf("projet_%s.nuee", taille)))
  }, function(ctx) {
    e <- banc_demande("enregistrer", list(projet = ctx$projet, chemin = ctx$chemin, session = ctx$session, version = "banc", ecraser = TRUE))
    o <- banc_demande("ouvrir", list(chemin = ctx$chemin))
    s <- jsonlite::fromJSON(banc_demande("session", list(projet = o$r$id))$r$session)
    refaits <- lapply(seq_len(nrow(s$modeles)), function(i) banc_demande("ajuster", list(projet = o$r$id, numero = s$modeles$numero[i], A = s$modeles$A[i])))
    list(temps = c(enregistrer = e$calcul, ouvrir = o$calcul + o$temps_json,
                   modeles = sum(vapply(refaits, function(x) x$calcul + x$journal + x$temps_json, 0))),
         octets = file.size(ctx$chemin) + sum(vapply(refaits, `[[`, 0, "octets")),
         sorties = list(fichier = readLines(ctx$chemin, encoding = "UTF-8", warn = FALSE), ouverture = banc_sans_chemins(o$r),
                        reponses = lapply(refaits, `[[`, "json"), modeles = lapply(refaits, function(x) banc_sans_ajustement(etat$modeles[[x$r$id]]$modele))))
  })
})

# ---------------------------------------------------------------- comparaison

# Deux résultats du même cas : "identique" (identical), "1e-12" (all.equal à 1e-12 près, pas
# identical), sinon la première différence qu'all.equal trouve.
banc_ecart <- function(a, b) {
  if (identical(a, b)) return("identique")
  ae <- all.equal(a, b, tolerance = 1e-12)
  if (isTRUE(ae)) return("1e-12")
  paste("DIFFÉRENT :", paste(utils::head(ae, 3), collapse = " ; "))
}

# ---------------------------------------------------------------- exécution

# L'état du serveur vidé (projets, modèles, journal, compteur des identifiants), puis gardé : chaque
# répétition d'un cas repart du même.
banc_vider <- function() {
  rm(list = ls(etat, all.names = TRUE), envir = etat)
  etat$projets <- list(); etat$modeles <- list(); etat$predictions <- list(); etat$compteur <- 0
  etat$annulations <- list(); etat$revisions <- list(); etat$paires <- list()
  etat$journal <- list(); etat$n_journal <- 0; etat$console_modifies <- character(0)
}
banc_instantane <- function() as.list(etat, all.names = TRUE)
banc_restaurer <- function(x) {
  rm(list = ls(etat, all.names = TRUE), envir = etat)
  list2env(x, etat)
}

choisis <- Filter(function(cas) {
  taille <- regmatches(cas$nom, regexpr("(petit|moyen|grand)", cas$nom))
  taille %in% banc_tailles && (!nzchar(banc_motif) || grepl(banc_motif, cas$nom))
}, banc_cas)
cat(sprintf("Moteur : %s\nR %s.%s, %s ; %d répétition(s) de %d cas\n\n", banc_racine, R.version$major, R.version$minor,
            utils::osVersion %||% R.version$platform, banc_repetitions, length(choisis)))

# (un premier passage, petit, que l'on ne compte pas : R compile les fonctions du moteur à leur
# premier appel)
local({
  banc_vider()
  p <- banc_projet(banc_spectres(30, 60), "rodage.csv")
  for (type in c("PCA", "PLS", "PLS-DA", "OPLS")) invisible(banc_ajuster(banc_q_spectres(p, type, 60)))
})

lignes <- list()
for (cas in choisis) {
  banc_vider()
  ctx <- cas$preparer()
  depart <- banc_instantane()
  passages <- list()
  sorties <- NULL
  for (i in seq_len(banc_repetitions)) {
    banc_restaurer(depart)
    invisible(gc(verbose = FALSE))
    if (!is.null(banc_profil) && i == 1) utils::Rprof(file.path(banc_profil, paste0(cas$nom, ".out")), interval = 0.01)
    t0 <- banc_horloge()
    r <- cas$executer(ctx)
    total <- banc_horloge() - t0
    if (!is.null(banc_profil) && i == 1) {
      utils::Rprof(NULL)
      s <- utils::summaryRprof(file.path(banc_profil, paste0(cas$nom, ".out")))
      utils::capture.output(print(utils::head(s$by.total, 60)), print(utils::head(s$by.self, 40)), file = file.path(banc_profil, paste0(cas$nom, ".txt")))
    }
    passages[[i]] <- c(total = total, r$temps)
    if (i == 1) sorties <- r$sorties
    octets <- r$octets
    rm(r)
  }
  temps <- do.call(rbind, passages)
  mediane <- apply(temps, 2, stats::median)
  statut <- ""
  if (!is.null(banc_garder)) saveRDS(sorties, file.path(banc_garder, paste0(cas$nom, ".rds")), compress = FALSE)
  if (!is.null(banc_comparer)) {
    f <- file.path(banc_comparer, paste0(cas$nom, ".rds"))
    statut <- if (!file.exists(f)) "(rien à comparer)" else {
      avant <- readRDS(f)
      ecarts <- vapply(names(sorties), function(k) banc_ecart(avant[[k]], sorties[[k]]), "")
      if (all(ecarts == "identique")) "identique" else paste(sprintf("%s : %s", names(ecarts), ecarts)[ecarts != "identique"], collapse = " | ")
    }
  }
  details <- paste(sprintf("%s %.2f", names(mediane)[-1], mediane[-1]), collapse = ", ")
  cat(sprintf("%-38s %8.2f s  (min %.2f, max %.2f)  %s  JSON %.2f Mo%s\n", cas$nom, mediane[["total"]], min(temps[, "total"]), max(temps[, "total"]),
              details, octets / 1e6, if (nzchar(statut)) paste0("\n  comparaison : ", statut) else ""))
  flush(stdout())
  lignes[[length(lignes) + 1]] <- data.frame(cas = cas$nom, total = mediane[["total"]], min = min(temps[, "total"]), max = max(temps[, "total"]),
                                             details = details, mo = octets / 1e6, comparaison = statut)
  rm(sorties, depart, ctx)
}
if (!is.null(banc_fichier_temps)) utils::write.csv(do.call(rbind, lignes), banc_fichier_temps, row.names = FALSE, fileEncoding = "UTF-8")
