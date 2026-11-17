# Vérifie le paquet de reproductibilité (moteur/R/paquet.R) sur le projet d'exemple, en français
# (vergers.json) puis en anglais (orchards.json) : ses quatre modèles de composantes, avec une
# régression, un arbre de décision, des k-moyennes, un modèle qui ne peut pas être refait, des unités
# et des descriptions, et en français un document reproductible (document_reproductible.R). Ce que
# l'archive contient : un dossier à son nom, le projet relu (ses données, ses modèles, les réponses
# du dialogue), les données en CSV et leur dictionnaire, le script R, le document et ses fichiers, le
# README, la licence et son texte, CITATION.cff et codemeta.json ; le script R lancé dans une session
# R neuve (Rscript --vanilla, sans le moteur), qui redonne les nombres du moteur pour chaque modèle,
# et le document refait de là ; les empreintes du manifeste, recalculées ici par un SHA-256 écrit à
# part (FIPS 180-4), vérifié d'abord sur ses exemples publiés, qui vérifie aussi que les textes des
# licences sont ceux de creativecommons.org ; les résultats clés déposés de chaque modèle (ceux de la
# séance, dans la langue du paquet, avec les versions de Nuée, de R et des paquets) et, avec son DOI, le
# code QR de son lien. Puis les réponses du dialogue refusées (titre, auteurs, ORCID, licence), en
# français et en anglais, un modèle dont le script ne s'écrit pas (les autres refaits sans lui), et les
# chemins qu'un document ne peut pas donner. Enfin le paquet rouvert par les demandes, comme
# l'interface le rouvre (serveur.R : ouvrir, paquet_ouvrir ; interface/relancer.js) : son manifeste
# vérifié, son projet ouvert sans fichier, chaque modèle ajusté de nouveau et comparé à ses résultats
# déposés (comparer_resultats) : tous les mêmes ; puis altéré : un CSV changé, un fichier en moins et
# un en plus, nommés par le manifeste ; une valeur changée dans les données du projet, qui fait une
# différence ; une archive dont un chemin sortirait de son dossier, une qui n'est pas un paquet.
#   Rscript moteur/tests/verifier_paquet.R

# Le serveur sans son écoute (comme verifier_etapes.R) : chaque expression de serveur.R jusqu'à
# l'interface HTTP, le moteur compris (charger.R), pour rouvrir le paquet par ses demandes.
racine_nuee <- normalizePath(".", winslash = "/")
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
dossier <- tempfile("paquet")
dir.create(dossier)
rscript <- file.path(R.home("bin"), "Rscript")

# ---------------------------------------------------------------- un SHA-256 écrit à part

# SHA-256 (FIPS 180-4) des fichiers `chemins`, tous à la fois (chaque opération sur le bloc k de
# chacun) : des mots de 32 bits en nombres de [0, 2^32), leurs opérations bit à bit sur deux
# moitiés de 16 bits. Indépendant de tools::sha256sum comme de sha256sum.
sha256_fichiers <- function(chemins) {
  M16 <- 65536
  M32 <- 4294967296
  et <- function(a, b) bitwAnd(a %/% M16, b %/% M16) * M16 + bitwAnd(a %% M16, b %% M16)
  xou <- function(a, b) bitwXor(a %/% M16, b %/% M16) * M16 + bitwXor(a %% M16, b %% M16)
  non <- function(a) M32 - 1 - a
  rotr <- function(x, n) (x %/% 2^n) + (x %% 2^n) * 2^(32 - n)
  K <- c(0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
         0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
         0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
         0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
         0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
         0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2)
  # chaque message complété : 0x80, des zéros, sa longueur en bits sur 64 bits (gros-boutiste)
  messages <- lapply(chemins, function(f) {
    o <- as.integer(readBin(f, "raw", file.size(f)))
    n <- length(o)
    bits <- n * 8
    longueur <- vapply(7:0, function(k) (bits %/% 256^k) %% 256, 0)
    c(o, 128L, rep(0L, (55 - n) %% 64), as.integer(longueur))
  })
  blocs <- vapply(messages, length, 0) / 64
  H <- matrix(c(0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19), 8, length(chemins))
  for (k in seq_len(max(blocs))) {
    actifs <- which(blocs >= k)
    W <- matrix(0, length(actifs), 64)
    for (j in seq_along(actifs)) {
      b <- messages[[actifs[j]]][(k - 1) * 64 + 1:64]
      W[j, 1:16] <- b[seq(1, 64, 4)] * 16777216 + b[seq(2, 64, 4)] * 65536 + b[seq(3, 64, 4)] * 256 + b[seq(4, 64, 4)]
    }
    for (t in 17:64) {
      s0 <- xou(xou(rotr(W[, t - 15], 7), rotr(W[, t - 15], 18)), W[, t - 15] %/% 8)
      s1 <- xou(xou(rotr(W[, t - 2], 17), rotr(W[, t - 2], 19)), W[, t - 2] %/% 1024)
      W[, t] <- (W[, t - 16] + s0 + W[, t - 7] + s1) %% M32
    }
    v <- H[, actifs, drop = FALSE]
    a <- v[1, ]; b <- v[2, ]; c <- v[3, ]; d <- v[4, ]; e <- v[5, ]; f <- v[6, ]; g <- v[7, ]; h <- v[8, ]
    for (t in 1:64) {
      S1 <- xou(xou(rotr(e, 6), rotr(e, 11)), rotr(e, 25))
      ch <- xou(et(e, f), et(non(e), g))
      t1 <- (h + S1 + ch + K[t] + W[, t]) %% M32
      S0 <- xou(xou(rotr(a, 2), rotr(a, 13)), rotr(a, 22))
      maj <- xou(xou(et(a, b), et(a, c)), et(b, c))
      t2 <- (S0 + maj) %% M32
      h <- g; g <- f; f <- e; e <- (d + t1) %% M32; d <- c; c <- b; b <- a; a <- (t1 + t2) %% M32
    }
    H[, actifs] <- (H[, actifs, drop = FALSE] + rbind(a, b, c, d, e, f, g, h)) %% M32
  }
  apply(H, 2, function(x) paste(sprintf("%08x", as.integer(x - (x >= 2^31) * M32)), collapse = ""))
}

cat("Un SHA-256 écrit à part : ses exemples publiés\n")
exemples <- c("", "abc", "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", strrep("a", 1000))
fichiers_exemples <- vapply(seq_along(exemples), function(k) { f <- file.path(dossier, sprintf("exemple%d.txt", k)); writeBin(charToRaw(exemples[k]), f); f }, "")
verifier("FIPS 180-4 : « », « abc », 448 bits, 1000 « a »",
         identical(sha256_fichiers(fichiers_exemples),
                   c("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
                     "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1", "41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3")))

# ---------------------------------------------------------------- les textes des licences

cat("Les textes des licences : ceux de creativecommons.org\n")
# Le legalcode.txt de chacune, tel que creativecommons.org le donnait le 17 novembre 2026 (celui de la
# CC0 1.0 est aussi celui que R livre, share/licenses/CC0-1.0) : leurs empreintes, sur leur texte en
# fins de ligne « \n » (une copie de Git sous Windows les écrit en « \r\n »).
officielles <- c("CC-BY-4.0" = "9ba9550ad48438d0836ddab3da480b3b69ffa0aac7b7878b5a0039e7ab429411",
                 "CC-BY-SA-4.0" = "28a9529c7d0bb4dc51f4bf5c116a3d16ef247a052f7591466768ddf563fd1cf5",
                 "CC-BY-NC-4.0" = "41003d4a74749c0220e33dd415042164b5a1093ed401f36277234f772d22d3d0",
                 "CC0-1.0" = "a2010f343487d3f7618affe54f789f5487602331c0a8d03f49e9a7c547cf0499")
sans_cr <- vapply(names(officielles), function(l) {
  f <- file.path("moteur", "licences", paste0(l, ".txt"))
  o <- readBin(f, "raw", file.size(f))
  copie <- file.path(dossier, paste0(l, ".txt"))
  writeBin(o[o != as.raw(13)], copie)
  copie
}, "")
cc0_r <- file.path(R.home("share"), "licenses", "CC0-1.0")
verifier(sprintf("les %d textes de moteur/licences sont octet pour octet ceux de creativecommons.org (legalcode.txt)%s", length(officielles),
                 if (file.exists(cc0_r)) ", la CC0 1.0 celle de R" else ""),
         identical(unname(sha256_fichiers(sans_cr)), unname(officielles)) && (!file.exists(cc0_r) || identical(sha256_fichiers(cc0_r), officielles[["CC0-1.0"]])))
# chaque LICENSE : qui donne les données et sous quelle licence, puis le texte de la licence, jusqu'à sa dernière ligne
meta_licence <- function(l) metadonnees_paquet(list(titre = "T", auteurs = list(list(prenom = "Alice", nom = "Exemple", orcid = "")), licence = l))
licences <- lapply(stats::setNames(names(officielles), names(officielles)), function(l) lignes_licence(meta_licence(l)))
titres <- c("CC-BY-4.0" = "Attribution 4.0 International", "CC-BY-SA-4.0" = "Attribution-ShareAlike 4.0 International",
            "CC-BY-NC-4.0" = "Attribution-NonCommercial 4.0 International", "CC0-1.0" = "Creative Commons Legal Code")
fins <- c(rep("Creative Commons may be contacted at creativecommons.org.", 3), "    this CC0 or use of the Work.")
verifier("chaque LICENSE : la licence nommée, son texte entier après elle, de son titre à sa dernière ligne",
         all(vapply(seq_along(licences), function(k) {
           x <- licences[[k]]
           l <- names(licences)[k]
           debut <- which(x == strrep("=", 71))[1] + 2
           identical(x[debut], titres[[l]]) && identical(utils::tail(x, 1), fins[k]) && any(grepl(LICENCES_PAQUET[[l]]$nom, x, fixed = TRUE)) &&
             identical(x[debut:length(x)], lire_lignes_paquet(sans_cr[[l]])[seq_len(length(x) - debut + 1)])
         }, TRUE)))

# ---------------------------------------------------------------- le projet d'exemple, et ce qu'on y ajoute

# Ce que le moteur donne d'un modèle, et ce que le script en a calculé, sous ses noms dans sa langue.
nombres <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.matrix(if (is.table(x)) unclass(x) else x)
  dimnames(x) <- NULL
  storage.mode(x) <- "double"
  x
}
meme <- function(a, b) !is.null(a) && !is.null(b) && isTRUE(all.equal(nombres(a), nombres(b), tolerance = 1e-10))

ecrire <- function(langue) {
  fr <- langue == "fr"
  options(nuee.langue = langue)
  p <- lire_projet(if (fr) "exemple/vergers.json" else "exemple/orchards.json")
  p$nom <- if (fr) "vergers.json" else "orchards.json"
  u <- function(...) numeros(p, c(...))
  sucre <- if (fr) "sucre" else "sugar"
  p <- decrire_variables(p, list(list(variable = u(sucre), unite = "°Brix", description = if (fr) "Sucres solubles, au réfractomètre" else "Soluble solids, by refractometer"),
                                 list(variable = u(if (fr) "poids" else "weight"), unite = "g")))
  note <- if (fr) "note_gout" else "taste_score"
  d_lm <- definir_modele(p, "LM", x = u(sucre, if (fr) "acidite" else "acidity", if (fr) "variete" else "variety"), y = u(note), echelle = "none", nom = "M5", titre = if (fr) "Note et sucre" else "Score and sugar")
  x4 <- u(sucre, if (fr) "fermete" else "firmness", if (fr) "acidite" else "acidity", if (fr) "amidon" else "starch")
  d_arbre <- definir_modele(p, "TREE", x = x4, y = u(note), echelle = "none", nom = "M6")
  d_km <- definir_modele(p, "KMEANS", x = x4, echelle = "UV", options = list(k = 3), nom = "M7")
  for (d in list(d_lm, d_arbre, d_km)) { d$app <- TRUE; d$number <- length(p$models) + 1; p$models <- c(p$models, list(d)) }
  # (comme un projet de la 1.1 : la source de ses données et une étape, sur l'ordinateur de l'auteur, dont le paquet ne garde rien)
  chez_auteur <- file.path(dossier, "chez l'auteur")
  dir.create(chez_auteur, showWarnings = FALSE)
  origine <- file.path(chez_auteur, paste0(tools::file_path_sans_ext(p$nom), ".csv"))
  ecrire_texte_paquet(origine, lignes_csv(tableau_projet(p)))
  p$source_donnees <- source_fichier(origine, "tableau")
  p$etapes <- list(list(demande = "decrire_variables", parametres = "{}", etiquette = "unités", langue = langue, date = format(Sys.time()), fichiers = list(fiche_fichier(origine))))
  p$empreinte_donnees <- empreinte_donnees(p)
  modeles <- lapply(seq_along(p$models), function(i) list(id = sprintf("m%d", i), definition = p$models[[i]], modele = ajuster(p, p$models[[i]]), erreur = NULL))
  # (un modèle qui ne peut pas être refait : une catégorie qu'il a vue a changé)
  perime <- list(id = "m8", definition = list(name = "M8", type = "PLS"), modele = NULL, erreur = if (fr) "les catégories de variete ont changé" else "the categories of variety changed")
  list(p = p, modeles = modeles, perime = perime)
}

meta_fr <- list(titre = "Pommes « bio » et conventionnelles", description = "Des données inventées : 120 pommes.\n\nUn second paragraphe.",
                auteurs = data.frame(prenom = c("Alice", "Bob"), nom = c("Exemple", "Essai"), orcid = c("https://orcid.org/0000-0002-1825-0097", "")),
                mots_cles = "pommes, ACP; PLS", licence = "CC-BY-4.0")
meta_en <- list(titre = "Organic and conventional apples", description = "Made-up data: 120 apples.",
                auteurs = list(list(prenom = "Bob", nom = "Example", orcid = "000000000000001X")), mots_cles = c("apples", "PCA"), licence = "CC0-1.0")

# lance un script dans une session neuve (ses graphiques sans fichier), puis garde les objets nommés
# (d'un environnement, ce qui n'est pas une fonction)
lanceur <- file.path(dossier, "lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".r <- lapply(mget(strsplit(.a[3], \",\")[[1]], ifnotfound = list(NULL)), function(x) if (is.environment(x)) Filter(Negate(is.function), as.list(x, all.names = TRUE)) else x)",
             "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, objets, ici) {
  sortie <- tempfile("sortie", dossier, ".rds")
  avant <- setwd(ici)
  on.exit(setwd(avant))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), shQuote(paste(objets, collapse = ","))), stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}

# (un code QR comme l'interface en donne un, interface/qr.js : le moteur l'écrit tel quel, sans le lire)
qr_essai <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" ?><svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" viewBox=\"-4 -4 29 29\" width=\"232\" height=\"232\">",
                   "<rect x=\"-4\" y=\"-4\" width=\"29\" height=\"29\" fill=\"#fff\"></rect><path d=\"M0 0L7 0L7 7L0 7Z\" fill=\"#000\"></path></svg>")
zips <- list()
for (langue in c("fr", "en")) {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  cat(sprintf("Le paquet du projet d'exemple, en %s\n", tx("français", "anglais")))
  x <- ecrire(langue)
  p <- x$p
  meta <- metadonnees_paquet(if (fr) meta_fr else meta_en)
  verifier("les réponses du dialogue nettoyées : l'ORCID d'une adresse ou sans tirets, les mots-clés d'un texte",
           if (fr) identical(meta$auteurs[[1]]$orcid, "0000-0002-1825-0097") && identical(meta$auteurs[[2]]$orcid, "") && identical(meta$mots_cles, c("pommes", "ACP", "PLS"))
           else identical(meta$auteurs[[1]]$orcid, "0000-0000-0000-001X") && identical(meta$licence, "CC0-1.0"))

  # le document (en français) : le script de M2, ses données et nuee.R, écrits par document_reproductible
  # dans un dossier de la session, et le .qmd que l'interface en écrit (rendu_document.js : versQmd)
  documents <- list()
  if (fr) {
    sortie_doc <- file.path(tempdir(), "document_paquet")
    dir.create(sortie_doc, showWarnings = FALSE)
    m2 <- x$modeles[[2]]
    texte_q2 <- code_aide_reproductible("fr")  # (seulement pour lire nuee_nombre ci-dessous)
    e_aide <- new.env(); eval(parse(text = texte_q2, encoding = "UTF-8"), envir = e_aide)
    attendu_q2 <- e_aide$nuee_nombre(m2$modele$statistiques$composantes$Q2cum[m2$modele$A])
    rep <- ecrire_document_reproductible(p, list(m2 = list(definition = m2$definition, modele = m2$modele)),
                                         list(list(modele = "m2", chemin = list("Q2"), genre = "valeur", forme = list(forme = "nombre", decimales = 2), texte = attendu_q2)),
                                         sortie_doc, "Mon article", "fr", "essai")
    qmd <- c("---", "title: \"Mon article\"", "lang: fr", "bibliography: Mon article.bib", "---", "", rep$installation, "",
             sprintf("Le Q² de M2 est `r %s` [@wold2001].", rep$references[[1]]$code), "", "![Une figure](Mon article_figures/figure-1.png)")
    png <- jsonlite::base64_enc(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 1:24)))
    documents <- list(list(nom = "Mon article", titre = "Mon article", qmd = paste(qmd, collapse = "\n"),
                           bib = "@article{wold2001,\n  title = {PLS-regression},\n  year = {2001}\n}\n", csl = list(nom = "apa.csl", texte = "<style/>"),
                           figures = list(list(chemin = "Mon article_figures/figure-1.png", png = paste0("data:image/png;base64,", png))),
                           fichiers = as.list(rep$fichiers), relatifs = as.list(rep$relatifs), note = ""))
  }
  zip <- file.path(dossier, langue, tx("Mon paquet.zip", "My package.zip"))
  dir.create(dirname(zip), recursive = TRUE, showWarnings = FALSE)
  # (en anglais, avec le DOI que Zenodo a réservé pour son brouillon, du préfixe de sandbox.zenodo.org, et le code QR
  # de son lien ; en français sans DOI : son code QR n'est pas écrit)
  r <- ecrire_paquet(p, c(x$modeles, list(x$perime)), zip, meta, documents, langue, "essai", session = "{\"version\":2}", doi = if (!fr) "https://doi.org/10.5072/zenodo.4242",
                     qr = qr_essai)
  zips[[langue]] <- zip
  verifier("l'archive est écrite, et chaque modèle y a son nom, sauf celui qui ne peut pas être refait",
           file.exists(zip) && identical(vapply(r$modeles, function(i) i$objet %||% NA_character_, ""), c(paste0("M", 1:7), NA)) &&
             identical(r$modeles[[8]]$erreur, x$perime$erreur) && !length(r$ecarts))
  ici <- file.path(dossier, langue, "contenu")
  utils::unzip(zip, exdir = ici)
  base <- tx("Mon paquet", "My package")
  racine <- file.path(ici, base)
  dans <- sort(list.files(ici, recursive = TRUE, all.files = TRUE), method = "radix")
  nom <- tx("vergers", "orchards")
  attendus <- sort(c(paste0(nom, ".nuee"), sprintf(tx("donnees/%s.csv", "data/%s.csv"), nom), sprintf(tx("donnees/%s - dictionnaire.csv", "data/%s - dictionary.csv"), nom),
                     tx("analyse.R", "analysis.R"), "README.md", "LICENSE", "CITATION.cff", "codemeta.json", "MANIFEST.sha256",
                     tx("resultats/resultats_cles.json", "results/key_results.json"), if (!fr) "qr-code.svg",
                     if (fr) c("documents/Mon article.qmd", "documents/Mon article.bib", "documents/apa.csl", "documents/Mon article_figures/figure-1.png",
                               paste0("documents/", rep$relatifs))), method = "radix")
  verifier(sprintf("un seul dossier, « %s », et ses %d fichiers%s", base, length(attendus),
                   if (!identical(dans, paste0(base, "/", attendus))) sprintf(" ; LUS : %s", paste(dans, collapse = ", ")) else ""),
           identical(dans, paste0(base, "/", attendus)) && identical(sort(as.character(r$fichiers), method = "radix"), attendus))

  # le projet
  q <- lire_projet(file.path(racine, paste0(nom, ".nuee")))
  verifier("le projet relu : les mêmes données, les mêmes modèles, les unités, les réponses du dialogue, la disposition de l'interface",
           identical(q$donnees, p$donnees) && identical(vapply(q$models, `[[`, "", "name"), paste0("M", 1:7)) &&
             identical(unite_variable(q, tx("sucre", "sugar")), "°Brix") && identical(metadonnees_gardees(q$paquet), meta) && identical(q$session, "{\"version\":2}"))
  # au format 3 de la 1.1, ses données venant du CSV du paquet, par son chemin dans le paquet : rien de
  # l'ordinateur de l'auteur (ni la source d'origine, ni ses étapes, ni le dossier temporaire où le paquet s'écrit)
  texte_nuee <- paste(readLines(file.path(racine, paste0(nom, ".nuee")), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  ici_r <- normalizePath(tempdir(), winslash = "/")
  csv_rel <- sprintf(tx("donnees/%s.csv", "data/%s.csv"), nom)
  verifier("le projet au format 3 : ses données venant de son CSV, par son chemin dans le paquet ; sans les étapes de l'auteur, ni aucun chemin de cet ordinateur",
           identical(as.integer(q$nuee$format), 3L) && identical(q$source_donnees$chemin, csv_rel) && identical(q$source_donnees$relatif, csv_rel) &&
             identical(q$source_donnees$lecteur, "tableau") && identical(q$source_donnees$reglages$identifiant, names(tableau_projet(p))[1]) &&
             !length(q$etapes) && isTRUE(donnees_conformes(q)) && !grepl(ici_r, texte_nuee, fixed = TRUE) &&
             !grepl(gsub("/", "\\\\\\\\", ici_r), texte_nuee, fixed = TRUE) && !grepl("chez l'auteur", texte_nuee, fixed = TRUE))
  # Édition › Mettre à jour les données, du paquet décompressé : son CSV relu, rien de changé ; décompressé
  # ailleurs, une valeur changée dans son CSV : c'est ce CSV-là qui est lu, et la valeur entre
  q$chemin <- file.path(racine, paste0(nom, ".nuee"))
  m <- mise_a_jour(q)
  o <- m$rapport$observations
  verifier("Mettre à jour les données, du paquet décompressé : son CSV relu, les mêmes observations et variables, aucune cellule changée",
           identical(normalizePath(m$projet$source_donnees$chemin, winslash = "/"), normalizePath(file.path(racine, csv_rel), winslash = "/")) &&
             o$ajoutees == 0 && o$retirees == 0 && o$cellules == 0 && !length(m$rapport$variables$nouvelles) && !length(m$rapport$variables$absentes))
  ailleurs <- file.path(dossier, langue, "ailleurs")
  dir.create(ailleurs)
  file.copy(racine, ailleurs, recursive = TRUE)
  copie <- file.path(ailleurs, basename(racine))
  f_csv <- file.path(copie, csv_rel)
  lu_copie <- utils::read.csv(f_csv, check.names = FALSE, na.strings = "", fileEncoding = "UTF-8", colClasses = "character")
  k <- match(tx("sucre", "sugar"), names(lu_copie))
  lu_copie[[k]][1] <- format(as.numeric(lu_copie[[k]][1]) + 1, digits = 15)
  ecrire_texte_paquet(f_csv, lignes_csv(lu_copie))
  q2 <- lire_projet(file.path(copie, paste0(nom, ".nuee")))
  q2$chemin <- file.path(copie, paste0(nom, ".nuee"))
  m2 <- mise_a_jour(q2)
  verifier("décompressé ailleurs, une valeur changée dans son CSV : la mise à jour lit ce CSV-là, une cellule changée",
           identical(normalizePath(m2$projet$source_donnees$chemin, winslash = "/"), normalizePath(f_csv, winslash = "/")) && m2$rapport$observations$cellules == 1)
  # les données et leur dictionnaire
  tab <- tableau_projet(p)
  lu <- utils::read.csv(file.path(racine, tx("donnees", "data"), paste0(nom, ".csv")), check.names = FALSE, na.strings = "", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  memes_colonnes <- vapply(seq_along(tab), function(k) if (is.numeric(tab[[k]])) isTRUE(all.equal(lu[[k]], tab[[k]], tolerance = 1e-14)) else identical(as.character(lu[[k]]), as.character(tab[[k]])), TRUE)
  verifier(sprintf("les données en CSV : %d observations, %d colonnes, les mêmes valeurs (les manquantes vides)", nrow(tab), ncol(tab)),
           identical(names(lu), names(tab)) && nrow(lu) == nrow(tab) && all(memes_colonnes))
  dico <- utils::read.csv(file.path(racine, tx("donnees", "data"), paste0(nom, tx(" - dictionnaire.csv", " - dictionary.csv"))), check.names = FALSE, na.strings = NULL, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  ligne <- function(v) dico[dico$variable == v, ]
  verifier("le dictionnaire : une ligne par colonne, son type, son unité, sa description, ses catégories dans leur ordre",
           identical(dico$variable, names(tab)) && identical(names(dico), c("variable", "type", tx("unité", "unit"), "description", tx("catégories", "categories"))) &&
             identical(ligne(names(tab)[1])$type, tx("identifiant", "identifier")) && identical(ligne(tx("sucre", "sugar"))[[3]], "°Brix") &&
             grepl(tx("réfractomètre", "refractometer"), ligne(tx("sucre", "sugar"))$description) && identical(ligne(tx("variete", "variety"))$type, "qualitative") &&
             identical(ligne(tx("variete", "variety"))[[5]], "Gala; Golden; Granny; Reinette") && identical(ligne(tx("poids", "weight"))[[3]], "g"))

  # le script, dans une session R neuve : les nombres du moteur pour chaque modèle
  script <- file.path(racine, tx("analyse.R", "analysis.R"))
  texte <- readLines(script, encoding = "UTF-8")
  verifier("le script : chaque modèle sous son nom, l'arbre et les k-moyennes chacun dans un environnement à lui, ses données dedans, rien du moteur",
           all(c("M6 <- local({", "M7 <- local({") %in% texte) && !any(grepl("^M[1-5] <- local", texte)) && !any(grepl("charger\\.R|load\\.R|read\\.csv|readRDS", texte)) &&
             any(grepl(tx("Pas dans ce script", "Not in this script"), texte)) && any(grepl("M8", texte)))
  # (son en-tête : celui d'un export vers R, l'avis de licence une fois pour tout le script, puis la licence de ses données)
  avis <- avis_licence_script(langue, "essai")
  debut_code <- which(!grepl("^\\s*(#|$)", texte))[1]
  ou <- which(vapply(seq_len(length(texte) - length(avis) + 1), function(k) identical(texte[k:(k + length(avis) - 1)], avis), NA))
  entete <- paste(sub("^#\\s?", "", texte[seq_len(debut_code - 1)]), collapse = " ")
  verifier("son en-tête, celui d'un export vers R : l'exception à la GPL et comment citer Nuée avec son DOI, une fois pour tout le script, avant le code ; puis la licence de ses données",
           length(ou) == 1 && ou < debut_code && sum(grepl("LICENSE-EXCEPTION.md", texte, fixed = TRUE)) == 1 &&
             sum(grepl(tx("merci de citer Nuée", "please cite Nuée"), texte, fixed = TRUE)) == 1 && grepl("Nuée (version essai) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.24263014", entete, fixed = TRUE) &&
             grepl(sprintf("%s (%s, LICENSE)", LICENCES_PAQUET[[meta$licence]]$court, LICENCES_PAQUET[[meta$licence]]$url), entete, fixed = TRUE) &&
             grepl(tx("L'avis ci-dessus porte sur le code.", "The notice above is about the code."), entete, fixed = TRUE))
  s <- lancer(script, paste0("M", 1:7), racine)
  if (!length(s)) {
    verifier(sprintf("le script tourne dans une session neuve (%s)", paste(utils::tail(attr(s, "messages"), 4), collapse = " / ")), FALSE)
  } else {
    n <- function(x) nom_script(x, langue)
    # (chacun dans sa liste, comme un script de toute la séance les garde : script.R, ecrire_script_seance)
    cv <- function(z) if (length(z)) do.call(rbind, lapply(z, `[`, c("SS", "DF", "MS", "F", "p")))
    composantes <- vapply(1:4, function(k) {
      m <- x$modeles[[k]]$modele
      st <- m$statistiques
      y <- s[[sprintf("M%d", k)]]
      obs <- y[[n("observations")]]
      pls <- !identical(m$type, "PCA")
      meme(m$T, y$m$T) && meme(st$composantes, y[[n("composantes")]]) && meme(st$observations$T2, obs$T2) && meme(st$observations$DModX, obs$DModX) &&
        meme(unlist(st$limites), y[[n("limites")]]) &&
        (!pls || meme(st$variables$VIP, y[[n("variables")]]$VIP) && meme(as.matrix(st$coefficients[paste("Coeff", m$y$noms)]), y$coefficients)) &&
        (!pls || !all(vapply(st$cv_anova, function(z) z$DF[3] > 0, TRUE)) || meme(cv(st$cv_anova), cv(y$cv_anova))) &&
        (!isTRUE(m$plsda) || meme(st$classification$vc$table, y$classification))
    }, TRUE)
    verifier(sprintf("M1 à M4 (ACP, PLS, PLS-DA, OPLS-DA) : les scores, R² et Q², T², DModX et leurs limites, VIP, coefficients, CV-ANOVA et classements du moteur%s",
                     if (!all(composantes)) sprintf(" ; DIFFÉRENTS : %s", paste(which(!composantes), collapse = ", ")) else ""), all(composantes))
    ml <- x$modeles[[5]]$modele
    rl <- s$M5
    verifier("M5 (régression linéaire) : ses coefficients, son AIC, son R² et son Q² sont ceux du moteur",
             meme(ml$resultat$coefficients$estimation, rl[[n("coefficients")]][[2]]) && meme(ml$resultat$AIC, rl[[n("criteres")]][["AIC"]]) &&
               meme(unlist(ml$resultat$r2), unlist(rl$r2)) && meme(ml$vc$Q2, rl$vc$Q2))
    ma <- x$modeles[[6]]$modele
    ra <- s$M6
    verifier("M6 (arbre, dans son environnement) : sa table d'élagage, ses valeurs ajustées et sa validation croisée sont celles du moteur",
             meme(ma$complet$cptable, ra[[nom_classement("elagage", langue)]]) && meme(ma$ajuste, ra[[nom_classement("ajuste", langue)]]) &&
               meme(ma$vc$prediction, ra[[nom_classement("vc", langue)]]))
    mk <- x$modeles[[7]]$modele
    rk <- s$M7
    verifier("M7 (k-moyennes, tirées au hasard d'une graine, dans son environnement) : ses groupes, ses centres et son R² sont ceux du moteur",
             identical(as.numeric(rk[[nom_classement("groupe", langue)]]), as.numeric(mk$groupe)) && meme(mk$r2, rk[[nom_classement("r2", langue)]]) &&
               meme(mk$centres, rk[[nom_classement("resultat", langue)]][[nom_classement("centres", langue)]]))
    # (et ce que l'export vers R vérifie de la régression, de l'arbre et des k-moyennes, sur ce que la session neuve a calculé)
    tab_s <- tableau_script(p, ids_script(list(x$modeles[[5]]$definition), p))
    ecarts <- c(ecart_regression(plan_modele(p, x$modeles[[5]]$definition, ml, tab_s), ml, list2env(rl), langue) %||% "", ecart_arbre(ma, list2env(ra), langue) %||% "",
                ecart_kmeans(mk, list2env(rk), langue) %||% "")
    verifier(sprintf("et ce que l'export vers R en compare : aucun écart%s", if (any(nzchar(ecarts))) sprintf(" ; %s", paste(ecarts[nzchar(ecarts)], collapse = " | ")) else ""),
             !any(nzchar(ecarts)))
  }
  if (fr) {
    # lancé tel que le README le dit : Rscript analyse.R, dans le dossier du paquet
    avant <- setwd(racine)
    messages <- suppressWarnings(system2(rscript, c("--vanilla", "analyse.R"), stdout = TRUE, stderr = TRUE))
    setwd(avant)
    verifier("Rscript analyse.R, dans le dossier du paquet, tourne jusqu'au bout (ses graphiques dans Rplots.pdf)",
             is.null(attr(messages, "status")) && file.exists(file.path(racine, "Rplots.pdf")))
    unlink(file.path(racine, "Rplots.pdf"))
    # le document : son premier bloc, puis sa valeur, refaits depuis documents/ dans une session neuve
    debut <- which(qmd == "```{r}")[1]
    fin <- debut + which(qmd[-seq_len(debut)] == "```")[1]
    refaire <- file.path(dossier, "refaire_document.R")
    writeLines(enc2utf8(c("setwd(commandArgs(TRUE)[1])", qmd[(debut + 1):(fin - 1)], sprintf("cat(%s)", rep$references[[1]]$code))), refaire, useBytes = TRUE)
    vu <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(refaire), shQuote(file.path(racine, "documents"))), stdout = TRUE, stderr = TRUE))
    verifier(sprintf("le document, depuis documents/ : son bloc refait M2, et sa valeur redonne celle de Nuée (%s)", attendu_q2),
             identical(utils::tail(vu, 1), attendu_q2))
  }

  # le README, la licence, CITATION.cff, codemeta.json
  readme <- readLines(file.path(racine, "README.md"), encoding = "UTF-8")
  verifier("le README : le titre, les auteurs et leur ORCID, chaque modèle, celui qui n'est pas refait, comment le relancer (R, rpart et sa version), l'ouvrir dans Nuée, vérifier les fichiers, le citer",
           readme[1] == paste("#", meta$titre) && any(grepl(meta$auteurs[[1]]$orcid, readme, fixed = TRUE)) &&
             all(vapply(paste0("**M", 1:7, "**"), function(m) any(grepl(m, readme, fixed = TRUE)), TRUE)) && any(grepl("**M8**", readme, fixed = TRUE)) &&
             any(grepl(sprintf("rpart %s", utils::packageDescription("rpart")$Version), readme, fixed = TRUE)) &&
             any(grepl(tx("`Rscript analyse.R`", "`Rscript analysis.R`"), readme, fixed = TRUE)) && any(grepl("https://nuee.io", readme, fixed = TRUE)) &&
             any(grepl("sha256sum -c MANIFEST.sha256", readme, fixed = TRUE)) && any(grepl("10.5281/zenodo.24263014", readme, fixed = TRUE)) &&
             any(grepl(if (fr) "## Refaire l'analyse avec R" else "## Redo the analysis with R", readme, fixed = TRUE)) &&
             (!fr || any(grepl("documents/Mon article.qmd", readme, fixed = TRUE))))
  verifier(if (fr) "sans DOI réservé, le README n'en donne pas d'autre que celui de Nuée"
           else "le DOI du paquet dans son README : en tête, dans sa citation, et le lien https://nuee.io/app/?doi=… qui l'ouvre dans Nuée pour navigateur",
           if (fr) !any(grepl("nuee.io/app", readme, fixed = TRUE)) && !any(grepl("^- \\*\\*DOI", readme)) && !any(grepl("10.5072", readme, fixed = TRUE))
           else "- **DOI:** [10.5072/zenodo.4242](https://doi.org/10.5072/zenodo.4242)" %in% readme &&
             any(grepl("[https://nuee.io/app/?doi=10.5072/zenodo.4242](https://nuee.io/app/?doi=10.5072/zenodo.4242) opens this package in Nuée", readme, fixed = TRUE)) &&
             any(grepl("[Data set]. Zenodo. https://doi.org/10.5072/zenodo.4242 Analysed with Nuée", readme, fixed = TRUE)))
  licence <- readLines(file.path(racine, "LICENSE"), encoding = "UTF-8")
  verifier(sprintf("la licence : qui la donne, puis son texte (%s)", meta$licence),
           licence[1] == meta$titre && if (fr) any(grepl("Attribution 4.0 International", licence)) && utils::tail(licence, 1) == "Creative Commons may be contacted at creativecommons.org." &&
             sprintf("Copyright (c) %s Alice Exemple and Bob Essai.", format(Sys.Date(), "%Y")) %in% licence
           else any(grepl("CC0 1.0 Universal", licence)) && any(grepl("dedicated to the public domain", licence)))
  cff <- readLines(file.path(racine, "CITATION.cff"), encoding = "UTF-8")
  structure_yaml <- grepl("^( *)(- )?[a-z-]+: .+$|^( *)- \".*\"$|^[a-z-]+:$|^( +)[a-z-]+:$", cff)
  verifier(sprintf("CITATION.cff : un jeu de données%s, ses auteurs avec leur ORCID, ses mots-clés, sa licence, Nuée (sa version, son DOI), R et rpart avec leurs versions", if (!fr) " et son DOI" else ""),
           all(structure_yaml) && cff[1] == "cff-version: 1.2.0" && "type: dataset" %in% cff && sprintf("title: %s", yaml_texte(meta$titre)) %in% cff &&
             sprintf("    orcid: \"https://orcid.org/%s\"", meta$auteurs[[1]]$orcid) %in% cff && sprintf("license: %s", meta$licence) %in% cff &&
             "    doi: \"10.5281/zenodo.24263014\"" %in% cff && "    version: \"essai\"" %in% cff &&
             sprintf("    version: \"%s.%s\"", R.version$major, R.version$minor) %in% cff && sprintf("    version: \"%s\"", utils::packageDescription("rpart")$Version) %in% cff &&
             any(grepl("title: \"rpart: ", cff, fixed = TRUE)) && all(sprintf("  - %s", yaml_texte(meta$mots_cles)) %in% cff) &&
             identical(grep("^doi: ", cff, value = TRUE), if (!fr) "doi: \"10.5072/zenodo.4242\"" else character(0)))
  cm <- jsonlite::fromJSON(file.path(racine, "codemeta.json"), simplifyVector = FALSE)
  verifier(sprintf("codemeta.json : CodeMeta 3.0, titre%s, auteurs (ORCID), mots-clés, licence, R et rpart avec leurs versions, Nuée par son DOI", if (!fr) ", le DOI du paquet en identifier" else ""),
           identical(cm[["@context"]], "https://w3id.org/codemeta/3.0") && identical(cm$name, meta$titre) &&
             identical(cm$author[[1]][["@id"]], paste0("https://orcid.org/", meta$auteurs[[1]]$orcid)) && identical(unlist(cm$keywords), meta$mots_cles) &&
             identical(cm$license, paste0("https://spdx.org/licenses/", meta$licence)) && identical(cm$programmingLanguage$version, paste(R.version$major, R.version$minor, sep = ".")) &&
             any(vapply(cm$softwareRequirements, function(z) identical(z$name, "rpart") && identical(z$version, utils::packageDescription("rpart")$Version), TRUE)) &&
             identical(cm$softwareSuggestions[[1]]$identifier, "https://doi.org/10.5281/zenodo.24263014") && identical(cm$softwareSuggestions[[1]]$version, "essai") &&
             identical(cm$identifier, if (!fr) "https://doi.org/10.5072/zenodo.4242"))

  # les résultats clés déposés : chaque modèle, ses entrées telles que la séance les a (resume_resultats, dans la
  # langue du paquet), relues de leur JSON égales à elles (comparer_resultats : aucune ne change), avec les
  # versions de Nuée, de R et des paquets ; le modèle qui ne peut pas être refait, avec pourquoi, sans entrées
  depot <- jsonlite::fromJSON(file.path(racine, tx("resultats/resultats_cles.json", "results/key_results.json")), simplifyVector = FALSE)
  session <- lapply(x$modeles, function(m) resume_resultats(p, m))
  memes <- vapply(seq_along(session), function(k) {
    d <- depot$modeles[[k]]
    cm <- comparer_resultats(session[[k]], d$entrees, langue)
    identical(d$nom, x$modeles[[k]]$definition$name) && cm$resume$changees == 0 && cm$resume$entrees == length(session[[k]]) &&
      identical(vapply(d$entrees, `[[`, "", "libelle"), vapply(session[[k]], `[[`, "", "libelle"))
  }, NA)
  avec_a <- vapply(depot$modeles[1:7], function(d) as.numeric(d$A), 0)
  verifier(sprintf("%s : les %d entrées de M1 à M7 telles que la séance les a, dans la langue du paquet, et M8 avec pourquoi il n'est pas refait ; Nuée, R %s et ses paquets (rpart %s)",
                   tx("resultats/resultats_cles.json", "results/key_results.json"), sum(lengths(session)), depot$R, depot$paquets$rpart %||% "?"),
           all(memes) && identical(depot$format, 1L) && identical(depot$nuee, "essai") && identical(depot$R, paste(R.version$major, R.version$minor, sep = ".")) &&
             identical(depot$plateforme, R.version$platform) && identical(depot$paquets$rpart, utils::packageDescription("rpart")$Version) &&
             identical(depot$date, format(Sys.Date())) && identical(depot$langue, langue) && identical(depot$doi, if (!fr) "10.5072/zenodo.4242") &&
             length(depot$modeles) == 8 && identical(depot$modeles[[8]]$erreur, x$perime$erreur) && is.null(depot$modeles[[8]]$entrees) &&
             all(avec_a[1:4] > 0) && all(avec_a[5:7] == 0) && identical(vapply(depot$modeles[1:4], `[[`, "", "type"), c("PCA", "PLS", "PLS-DA", "OPLS-DA")))
  verifier(if (fr) "le README dit ce que sont les résultats déposés, et comment Nuée relance le paquet ; sans DOI, ni code QR ni son fichier"
           else "le README dit ce que sont les résultats déposés et le code QR (qr-code.svg, tel que l'interface l'a donné), et comment Nuée relance le paquet",
           any(grepl(tx("| `resultats/resultats_cles.json` | Les résultats clés de chaque modèle", "| `results/key_results.json` | Each model's key results"), readme, fixed = TRUE)) &&
             any(grepl(tx("Fichier › Ouvrir un paquet de reproductibilité…", "File › Open a reproducibility package…"), readme, fixed = TRUE)) &&
             if (fr) !file.exists(file.path(racine, "code-qr.svg")) && !any(grepl("QR", readme, fixed = TRUE))
             else identical(readChar(file.path(racine, "qr-code.svg"), 1e5, useBytes = TRUE), qr_essai) && any(grepl("| `qr-code.svg` | The QR code of the link", readme, fixed = TRUE)) &&
               any(grepl("its QR code is `qr-code.svg`", readme, fixed = TRUE)))

  # le manifeste : chaque fichier sauf lui, une fois, et son empreinte recalculée ici
  manifeste <- readLines(file.path(racine, "MANIFEST.sha256"), encoding = "UTF-8")
  chemins <- sub("^[0-9a-f]{64}  ", "", manifeste)
  empreintes <- sub("  .*$", "", manifeste)
  verifier("le manifeste : « empreinte  chemin », chaque fichier sauf lui une fois, dans l'ordre de leurs octets",
           all(grepl("^[0-9a-f]{64}  [^ ].*$", manifeste)) && identical(chemins, setdiff(attendus, "MANIFEST.sha256")))
  recalculees <- sha256_fichiers(file.path(racine, chemins))
  verifier(sprintf("ses %d empreintes SHA-256 sont celles de chaque fichier, recalculées ici", length(chemins)), identical(recalculees, empreintes))
}

cat("Les réponses du dialogue refusées, et les chemins d'un document\n")
for (langue in c("fr", "en")) {
  options(nuee.langue = langue)
  refus <- function(x) tryCatch({ metadonnees_paquet(x); "" }, error = function(e) conditionMessage(e))
  base <- list(titre = "T", auteurs = list(list(prenom = "A", nom = "B", orcid = "")), licence = "CC-BY-4.0")
  avec <- function(...) { x <- base; v <- list(...); x[names(v)] <- v; x }
  messages <- c(refus(avec(titre = "  ")), refus(avec(auteurs = list())), refus(avec(auteurs = list(list(prenom = "A", nom = "", orcid = "")))),
                refus(avec(auteurs = list(list(prenom = "A", nom = "B", orcid = "0000-0002-1825-0098")))),
                refus(avec(auteurs = list(list(prenom = "A", nom = "B", orcid = "1234")))), refus(avec(licence = "MIT")),
                tryCatch({ doi_paquet("zenodo.4242"); "" }, error = function(e) conditionMessage(e)))
  attendus_refus <- if (langue == "fr") c("pas de titre", "pas d'auteur", "sans nom de famille", "pas un ORCID : 0000-0002-1825-0098", "pas un ORCID : 1234", "Licence inconnue : MIT",
                                          "Ce n'est pas un DOI : zenodo.4242")
                    else c("no title", "no author", "without a family name", "isn't an ORCID: 0000-0002-1825-0098", "isn't an ORCID: 1234", "Unknown licence: MIT",
                           "This isn't a DOI: zenodo.4242")
  verifier(sprintf("en %s : un titre vide, aucun auteur, un auteur sans nom, un ORCID à la clé fausse ou trop court, une licence inconnue, un DOI qui n'en est pas un",
                   if (langue == "fr") "français" else "anglais"),
           all(mapply(function(m, a) grepl(a, m, fixed = TRUE), messages, attendus_refus)) && identical(refus(base), "") &&
             identical(vapply(c("doi:10.5281/zenodo.42", "https://doi.org/10.5281/zenodo.42", " 10.5281/zenodo.42 "), doi_paquet, "", USE.NAMES = FALSE), rep("10.5281/zenodo.42", 3)) &&
             is.null(doi_paquet("")) && is.null(doi_paquet(NULL)))
}
options(nuee.langue = "fr")
x <- ecrire("fr")
p <- x$p
meta <- metadonnees_paquet(meta_fr)
# un modèle dont le script ne s'écrit pas (ici, sans ses résultats) : nommé avec son erreur, les autres refaits sans lui
casse <- x$modeles[1:2]
casse[[2]]$modele <- list()
r <- ecrire_paquet(p, casse, file.path(dossier, "casse.zip"), meta, list(), "fr", "essai")
verifier("un modèle dont le script ne s'écrit pas : nommé avec son erreur, l'autre toujours refait",
         identical(r$modeles[[1]]$objet, "M1") && is.null(r$modeles[[2]]$objet) && is.character(r$modeles[[2]]$erreur) && nzchar(r$modeles[[2]]$erreur))
# un R d'avant 4.5, sans tools::sha256sum (R 4.2 de Debian 12, R 4.1 d'Ubuntu 22.04) : empreinte_sha y prend
# un MD5 ; le manifeste reste en SHA-256, par l'outil du système (sha256sum, sinon shasum -a 256) quand
# Sys.which le trouve, puis, ces outils cachés à Sys.which, par sha256_r ; chaque fois retrouvé par
# l'implémentation de cette vérification. (sha256_r compte ses appels : aucun quand l'outil a servi.)
empreinte_r45 <- empreinte_sha
sha256_r_moteur <- sha256_r
appels_r <- 0
empreinte_sha <- function(chemin = NULL, octets = NULL, md5 = FALSE) empreinte_r45(chemin, octets, md5 = TRUE)
sha256_r <- function(octets) { appels_r <<- appels_r + 1; sha256_r_moteur(octets) }
outil <- Filter(nzchar, Sys.which(c("sha256sum", "shasum")))
manifeste_r41 <- function(nom) {
  appels_r <<- 0
  r <- ecrire_paquet(p, x$modeles[1:2], file.path(dossier, paste0(nom, ".zip")), meta, list(), "fr", "essai")
  ici <- file.path(dossier, nom)
  utils::unzip(r$chemin, exdir = ici)
  manifeste <- readLines(file.path(ici, nom, "MANIFEST.sha256"), encoding = "UTF-8")
  chemins <- sub("^[0-9a-f]{64}  ", "", manifeste)
  list(n = length(manifeste), appels = appels_r,
       ok = length(manifeste) == length(r$fichiers) - 1 && all(grepl("^[0-9a-f]{64}  ", manifeste)) &&
         identical(sha256_fichiers(file.path(ici, nom, chemins)), sub("  .*$", "", manifeste)))
}
par_outil <- manifeste_r41("R 4.1")
Sys.which <- function(names) { x <- base::Sys.which(names); x[names %in% c("sha256sum", "shasum")] <- ""; x }
sans_outil <- manifeste_r41("R 4.1 sans outil")
rm(Sys.which)
empreinte_sha <- empreinte_r45
sha256_r <- sha256_r_moteur
verifier(sprintf("sans tools::sha256sum (un R d'avant 4.5), par %s : le manifeste en SHA-256, ses %d empreintes celles de chaque fichier",
                 if (length(outil)) sprintf("l'outil du système (%s), sans sha256_r", names(outil)[1]) else "sha256_r (aucun outil ici)", par_outil$n),
         par_outil$ok && (!length(outil) || par_outil$appels == 0))
verifier(sprintf("… et sans outil du système (caché à Sys.which) : par sha256_r, en R seul, une fois par fichier, les mêmes %d empreintes justes", sans_outil$n),
         sans_outil$ok && sans_outil$appels == sans_outil$n)
refuse <- function(doc) tryCatch({ ecrire_paquet(p, list(), file.path(dossier, "refus.zip"), meta, list(doc), "fr", "essai"); FALSE }, error = function(e) TRUE)
dehors <- file.path(dossier, "dehors.txt")
writeLines("x", dehors)
verifier("un document ne peut rien écrire hors du paquet (« .. », un chemin absolu), ni y mettre un fichier hors de la session du moteur",
         refuse(list(nom = "a", qmd = "", figures = list(list(chemin = "../x.png", png = "")))) &&
           refuse(list(nom = "a", qmd = "", figures = list(list(chemin = "C:/x.png", png = "")))) &&
           refuse(list(nom = "../a", qmd = "")) &&
           (startsWith(normalizePath(dehors, winslash = "/"), paste0(normalizePath(tempdir(), winslash = "/"), "/")) ||
              refuse(list(nom = "a", qmd = "", fichiers = list(dehors), relatifs = list("a_R/dehors.txt")))) &&
           refuse(list(nom = "a", qmd = "", fichiers = list(file.path(R.home(), "COPYING")), relatifs = list("a_R/COPYING"))) &&
           !file.exists(file.path(dossier, "refus.zip")))
refus_qr <- function(q) tryCatch({ ecrire_paquet(p, list(), file.path(dossier, "refus_qr.zip"), meta, list(), "fr", "essai", doi = "10.5072/zenodo.1", qr = q); "" }, error = conditionMessage)
verifier("un code QR qui n'est pas une image SVG (du HTML, un script, un lien) est refusé, et rien n'est écrit",
         all(grepl("pas une image SVG", c(refus_qr("<html><body>QR</body></html>"), refus_qr("<svg><script>alert(1)</script></svg>"),
                                          refus_qr("<svg><a href=\"https://exemple.invalid\"><path d=\"M0 0\"/></a></svg>"), refus_qr("<svg onload=\"x()\"></svg>")), fixed = TRUE)) &&
           !file.exists(file.path(dossier, "refus_qr.zip")))

# ---------------------------------------------------------------- le paquet rouvert et relancé

# Les demandes comme l'interface les fait (traiter_demande : la demande en JSON, la réponse lue de son JSON
# comme l'interface la lit, ses tableaux des listes) ; une demande refusée rend son erreur et ce qu'elle porte.
demande <- function(nom, q = list(), langue = "fr") {
  corps <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA))))
  r <- traiter_demande(nom, corps, langue)
  x <- jsonlite::fromJSON(r$body, simplifyVector = FALSE)
  if (r$status != 200L) return(structure(list(), erreur = x))
  x
}
# Le paquet `zip` rouvert (paquet_ouvrir), puis chaque modèle déposé ajusté de nouveau dans son projet, par son
# numéro, avec le nombre de composantes de son ajustement déposé, et ses résultats clés comparés aux déposés
# (comparer_resultats), comme interface/relancer.js le fait ; `differe` : une valeur déposée qui n'est pas
# redonnée (une entrée que seul le nouvel ajustement a n'en est pas une).
relancer <- function(zip, langue) {
  o <- demande("paquet_ouvrir", list(chemin = zip, version = "essai"), langue)
  if (!length(o)) return(list(o = attr(o, "erreur"), comparaisons = list()))
  comparaisons <- list()
  for (dm in o$depose$modeles) {
    if (is.null(dm$entrees)) next
    d <- Find(function(m) identical(m$nom, dm$nom), o$projet$modeles)
    a <- demande("ajuster", c(list(projet = o$projet$id, numero = d$numero), if (isTRUE(dm$A > 0)) list(A = dm$A)), langue)
    k <- demande("resultats_cles", list(modele = a$id), langue)
    comparaisons[[dm$nom]] <- demande("comparer_resultats", list(avant = list(entrees = dm$entrees), apres = list(entrees = k$entrees), langue = langue), langue)
  }
  list(o = o, comparaisons = comparaisons)
}
differe <- function(cm) any(vapply(cm$entrees, function(e) isTRUE(e$change) && !isTRUE(e$nouveau), NA))
empreinte_zip <- function(f) unname(tools::md5sum(f))

for (langue in c("fr", "en")) {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  cat(sprintf("Le paquet rouvert et relancé, en %s (serveur.R : ouvrir, paquet_ouvrir)\n", tx("français", "anglais")))
  zip <- zips[[langue]]
  avant <- empreinte_zip(zip)
  o <- demande("ouvrir", list(chemin = zip), langue)
  verifier("Fichier › Ouvrir sur le .zip : la réponse dit que c'est un paquet, à rouvrir par paquet_ouvrir, sans rien ouvrir",
           identical(o$paquet_ouvrir$chemin, zip) && identical(o$paquet_ouvrir$nom, basename(zip)) && is.null(o$id))
  r <- relancer(zip, langue)
  p <- r$o$projet
  m <- r$o$manifeste
  base <- tx("Mon paquet", "My package")
  et <- demande("etapes", list(projet = p$id), langue)
  verifier(sprintf("rouvert : décompressé dans la séance, ses %d fichiers ceux de son manifeste ; son projet « %s » sans fichier, pas modifié, d'où il vient gardé ; sa source le CSV décompressé ; l'archive inchangée",
                   m$fichiers, p$nom),
           isTRUE(m$present) && isTRUE(m$intact) && m$fichiers >= 10 && !length(m$manquants) && !length(m$changes) && !length(m$ajoutes) &&
             identical(p$nom, paste0(base, ".nuee")) && is.null(p$chemin) && is.null(p$chemin_enregistrement) && isFALSE(p$modifie) &&
             identical(p$depuis_paquet$titre, tx(meta_fr$titre, meta_en$titre)) && identical(p$depuis_paquet$fichier, basename(zip)) &&
             identical(r$o$doi, if (!fr) "10.5072/zenodo.4242") && identical(r$o$titre, tx(meta_fr$titre, meta_en$titre)) &&
             startsWith(normalizePath(r$o$dossier, winslash = "/"), normalizePath(tempdir(), winslash = "/")) &&
             isTRUE(et$source$existe) && isFALSE(et$source$modifie) && startsWith(normalizePath(et$source$chemin, winslash = "/"), normalizePath(r$o$dossier, winslash = "/")) &&
             identical(r$o$ici$nuee, "essai") && identical(r$o$depose$nuee, "essai") && identical(r$o$ici$plateforme, R.version$platform) && !length(r$o$paquets_differents) &&
             identical(empreinte_zip(zip), avant))
  n_entrees <- sum(vapply(r$comparaisons, function(cm) cm$resume$entrees, 0))
  verifier(sprintf("relancé : M1 à M7 ajustés de nouveau redonnent leurs %d résultats déposés (comparer_resultats : aucun ne change) ; M8, sans résultats déposés, n'est pas comparé",
                   n_entrees),
           identical(names(r$comparaisons), paste0("M", 1:7)) && !any(vapply(r$comparaisons, differe, NA)) &&
             all(vapply(r$comparaisons, function(cm) cm$resume$changees == 0 && !length(cm$phrases), NA)) && n_entrees > 300)
  demande("fermer_projet", list(projet = p$id), langue)
}

# altéré : décompressé, changé, compressé de nouveau (sans refaire son manifeste)
cat("Le paquet altéré\n")
alterer <- function(nom, f) {
  ici <- file.path(dossier, "altere", nom)
  dir.create(ici, recursive = TRUE)
  zip::unzip(zips$en, exdir = ici)
  f(file.path(ici, "My package"))
  z <- file.path(dossier, "altere", paste0(nom, ".zip"))
  suppressWarnings(zip::zip(z, files = list.files(ici), root = ici))
  z
}
changer_csv <- function(racine) {
  f <- file.path(racine, "data", "orchards.csv")
  lu <- utils::read.csv(f, check.names = FALSE, na.strings = "", fileEncoding = "UTF-8", colClasses = "character")
  lu$sugar[1] <- format(as.numeric(lu$sugar[1]) + 1, digits = 15)
  ecrire_texte_paquet(f, lignes_csv(lu))
}
r <- relancer(alterer("csv", changer_csv), "en")
m <- r$o$manifeste
verifier("une valeur changée dans son CSV : le manifeste le nomme (data/orchards.csv changé), le projet s'ouvre et ses modèles, de ses propres données, redonnent les résultats déposés",
         isTRUE(m$present) && isFALSE(m$intact) && identical(unlist(m$changes), "data/orchards.csv") && !length(m$manquants) && !length(m$ajoutes) &&
           length(r$comparaisons) == 7 && !any(vapply(r$comparaisons, differe, NA)))
r <- relancer(alterer("fichiers", function(racine) {
  unlink(file.path(racine, "LICENSE"))
  writeLines("ajouté", file.path(racine, "notes.txt"))
}), "en")
m <- r$o$manifeste
verifier("un fichier en moins (LICENSE) et un en plus (notes.txt) : le manifeste nomme l'un manquant et l'autre ajouté",
         isFALSE(m$intact) && identical(unlist(m$manquants), "LICENSE") && identical(unlist(m$ajoutes), "notes.txt") && !length(m$changes))
r <- relancer(alterer("donnees", function(racine) {
  f <- file.path(racine, "orchards.nuee")
  q <- lire_projet(f)
  k <- nom_numero(numeros(q, "sugar"))
  q$donnees[1, k] <- q$donnees[1, k] + 1
  enregistrer_projet(q, f, "essai", dossier = NULL)
}), "en")
m <- r$o$manifeste
changes <- names(Filter(differe, r$comparaisons))
coef_sucre <- Find(function(e) identical(e$cle, "coef.sugar"), r$comparaisons$M5$entrees)
verifier(sprintf("une valeur changée dans les données du projet (le sucre de la première pomme) : le manifeste nomme orchards.nuee, et le relancer diffère (%s ; le coefficient du sucre de M5 de %s à %s)",
                 paste(changes, collapse = ", "), format(coef_sucre$avant %||% NA, digits = 6), format(coef_sucre$apres %||% NA, digits = 6)),
         identical(unlist(m$changes), "orchards.nuee") && length(changes) >= 5 && "M5" %in% changes && isTRUE(coef_sucre$change) &&
           sum(vapply(r$comparaisons, function(cm) cm$resume$changees, 0)) > 20)
# (le paquet et, à côté de son dossier, un fichier que l'archive nomme ../dehors.txt)
glisse <- alterer("glisse", function(racine) NULL)
writeLines("x", file.path(dossier, "altere", "dehors.txt"))
unlink(glisse)
suppressWarnings(zip::zip(glisse, files = c("My package", "../dehors.txt"), root = file.path(dossier, "altere", "glisse")))
r <- relancer(glisse, "en")
verifier("une archive dont un chemin sortirait de son dossier (../dehors.txt) est refusée, sans rien décompresser",
         "../dehors.txt" %in% zip::zip_list(glisse)$filename && grepl("outside its folder: ../dehors.txt", r$o$erreur %||% "", fixed = TRUE) &&
           !file.exists(file.path(tempdir(), "dehors.txt")))
# des écarts aux derniers chiffres : les résultats déposés retouchés (le Q² de M2 de 1e-9 en relatif, une
# valeur p de M5 de 4e-7, le R² de M7 de 1e-5 en relatif), le manifeste refait pour eux : M2 et M5 redonnent
# les résultats déposés à 6 chiffres significatifs près, avec leur plus grand écart ; M7 diffère ; les
# autres sont identiques
retoucher <- function(racine) {
  f <- file.path(racine, "results", "key_results.json")
  d <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  noms <- vapply(d$modeles, `[[`, "", "nom")
  entree <- function(m, test) which(vapply(d$modeles[[match(m, noms)]]$entrees, test, NA))[1]
  k <- entree("M2", function(e) identical(e$cle, "Q2"))
  d$modeles[[match("M2", noms)]]$entrees[[k]]$valeur <- d$modeles[[match("M2", noms)]]$entrees[[k]]$valeur * (1 + 1e-9)
  k <- entree("M5", function(e) !is.null(e$p) && e$p < 0.5)
  d$modeles[[match("M5", noms)]]$entrees[[k]]$p <- d$modeles[[match("M5", noms)]]$entrees[[k]]$p + 4e-7
  k <- entree("M7", function(e) identical(e$cle, "R2"))
  d$modeles[[match("M7", noms)]]$entrees[[k]]$valeur <- d$modeles[[match("M7", noms)]]$entrees[[k]]$valeur * (1 + 1e-5)
  ecrire_texte_paquet(f, as.character(jsonlite::toJSON(d, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")))
  m <- lire_lignes_paquet(file.path(racine, "MANIFEST.sha256"))
  m[endsWith(m, "  results/key_results.json")] <- paste0(empreintes_sha256(f), "  results/key_results.json")
  ecrire_texte_paquet(file.path(racine, "MANIFEST.sha256"), m)
}
r <- relancer(alterer("ecarts", retoucher), "en")
verdicts <- vapply(r$comparaisons, function(cm) cm$resume$verdict, "")
ecart <- function(nom) r$comparaisons[[nom]]$resume
verifier(sprintf("des écarts aux derniers chiffres : M2 et M5 à 6 chiffres significatifs près (leur plus grand écart : %s, %s ; %s, %s), M7 diffère (%s), les autres identiques ; le manifeste intact",
                 format(ecart("M2")$ecart_max, digits = 3), ecart("M2")$cle_ecart_max, format(ecart("M5")$ecart_max, digits = 3), ecart("M5")$cle_ecart_max, format(ecart("M7")$ecart_max, digits = 3)),
         isTRUE(r$o$manifeste$intact) && identical(unname(verdicts), c("identique", "six_chiffres", "identique", "identique", "six_chiffres", "identique", "differe")) &&
           abs(ecart("M2")$ecart_max / 1e-9 - 1) < 0.01 && identical(ecart("M2")$cle_ecart_max, "Q2") && identical(ecart("M2")$partie_ecart_max, "valeur") &&
           abs(ecart("M5")$ecart_max / 4e-7 - 1) < 1e-6 && identical(ecart("M5")$partie_ecart_max, "p") &&
           abs(ecart("M7")$ecart_max / 1e-5 - 1) < 0.01 && identical(ecart("M7")$cle_ecart_max, "R2"))
# le verdict, entrée par entrée (resultats_cles.R : verdict_comparaison) : en relatif, rapporté à 1e-10 près de
# zéro ; une valeur p en absolu ; un intervalle ; une entrée disparue, une catégorie changée ; une nouvelle
v <- function(a, b) comparer_resultats(a, b, "en")$resume
e <- function(cle, valeur, ...) entree_resultat(cle, cle, "estimations", valeur, ...)
base <- list(e("a", 2.5, ic = c(2, 3), p = 0.04), e("b", 1234.5))
cas <- list(
  identique = v(base, base),
  relatif = v(base, list(e("a", 2.5 * (1 + 5e-7), ic = c(2, 3), p = 0.04), e("b", 1234.5))),
  trop = v(base, list(e("a", 2.5 * (1 + 2e-6), ic = c(2, 3), p = 0.04), e("b", 1234.5))),
  zero = v(list(e("c", 3e-17)), list(e("c", -2e-17))),
  zero_trop = v(list(e("c", 3e-17)), list(e("c", 3e-15))),
  p_queue = v(list(e("d", 6, p = 3e-9)), list(e("d", 6, p = 3.0002e-9))),
  p_trop = v(list(e("d", 2, p = 0.04)), list(e("d", 2, p = 0.0401))),
  ic = v(base, list(e("a", 2.5, ic = c(2 * (1 + 1e-8), 3), p = 0.04), e("b", 1234.5))),
  disparue = v(base, base[1]),
  nouvelle = v(base, c(base, list(e("n", 7)))),
  categorie = v(list(entree_resultat("k", "k", "ajustement", 1, classe = "A")), list(entree_resultat("k", "k", "ajustement", 1, classe = "B"))))
attendus_v <- c(identique = "identique", relatif = "six_chiffres", trop = "differe", zero = "six_chiffres", zero_trop = "differe", p_queue = "six_chiffres",
                p_trop = "differe", ic = "six_chiffres", disparue = "differe", nouvelle = "identique", categorie = "differe")
obtenus_v <- vapply(cas, `[[`, "", "verdict")
verifier(sprintf("le verdict : à 1e-6 près en relatif (5e-7 oui, 2e-6 non), près de zéro en absolu à 1e-10 près (3e-17 et -2e-17 oui, 3e-15 non), une valeur p en absolu (3e-9 à 2e-13 près oui, 0,04 et 0,0401 non), un intervalle ; une entrée disparue ou une catégorie changée diffère, une nouvelle ne compte pas%s",
                 if (!identical(obtenus_v, attendus_v)) sprintf(" ; OBTENUS : %s", paste(names(obtenus_v), obtenus_v, sep = "=", collapse = ", ")) else ""),
         identical(obtenus_v, attendus_v) && abs(cas$relatif$ecart_max / 5e-7 - 1) < 1e-6 && identical(cas$relatif$cle_ecart_max, "a") && is.infinite(cas$disparue$ecart_max) &&
           identical(c(cas$relatif$partie_ecart_max, cas$ic$partie_ecart_max, cas$p_queue$partie_ecart_max), c("valeur", "ic", "p")))
tableau <- file.path(dossier, "altere", "tableau.zip")
zip::zip(tableau, files = "orchards.csv", root = file.path(dossier, "altere", "csv", "My package", "data"))
r <- relancer(tableau, "en")
o <- demande("ouvrir", list(chemin = tableau), "en")
verifier("une archive qui n'est pas un paquet : refusée par paquet_ouvrir (pas_un_paquet), et Fichier › Ouvrir ne la prend pas pour un paquet",
         grepl("isn't a reproducibility package", r$o$erreur %||% "", fixed = TRUE) && isTRUE(r$o$pas_un_paquet) && is.null(o$paquet_ouvrir))

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
