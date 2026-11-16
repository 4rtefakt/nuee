# Vérifie la description du protocole du moteur (moteur/protocole.json, docs/protocole.md) : chaque
# demande que le moteur sert (les DEMANDES de serveur.R, avec les demande_analyse_<nom> des analyses)
# y est décrite, et chaque demande décrite existe, pour qu'aucune ne s'ajoute sans sa description ;
# la description se tient (sa structure, ses types nommés, ses erreurs communes, ses ensembles de
# paramètres, ses versions) ; une demande dite « annulable » est bien celle qui garde un retour en
# arrière (memoriser), et les autres non ; la demande protocole donne la version, celle de Nuée, les
# demandes et, avec complet, la description telle que le JSON de la réponse la rend ;
# docs/protocole.md est la page qu'en écrit docs/ecrire_protocole.R.
#   Rscript moteur/tests/verifier_protocole.R   (depuis la racine du dépôt)

echecs <- 0
verifier <- function(nom, ok, details = character(0)) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) {
    if (length(details)) cat(paste0("            ", details), sep = "\n")
    echecs <<- echecs + 1
  }
}

# Le serveur sans son écoute : chaque expression de serveur.R jusqu'au démarrage de httpuv, sa
# racine et ses arguments donnés ici (comme banc_vitesse.R).
racine_nuee <- normalizePath(".", winslash = "/")
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
options(nuee.langue = "en")

fichier <- file.path(racine_nuee, "moteur", "protocole.json")
description <- jsonlite::fromJSON(fichier, simplifyVector = FALSE)
decrites <- names(description$demandes)

cat("Chaque demande du moteur est décrite, et rien d'autre\n")
manquantes <- setdiff(names(DEMANDES), decrites)
verifier(sprintf("les %d demandes du moteur sont décrites dans moteur/protocole.json", length(DEMANDES)), !length(manquantes),
         sprintf("sans description : %s (ajoutez-la à moteur/protocole.json, puis Rscript docs/ecrire_protocole.R)", paste(manquantes, collapse = ", ")))
inconnues <- setdiff(decrites, names(DEMANDES))
verifier("chaque demande décrite existe", !length(inconnues), sprintf("décrites sans exister : %s", paste(inconnues, collapse = ", ")))
verifier("chaque demande n'est décrite qu'une fois", !anyDuplicated(decrites), paste(decrites[duplicated(decrites)], collapse = ", "))

# ---------------------------------------------------------------- la structure

TYPES_DE_BASE <- c("string", "number", "integer", "boolean", "object", "null", "any")
EFFETS <- c("aucun", "seance", "projet", "annulable")
CLES <- list(
  racine = c("version", "versions", "groupes", "transport", "types", "ensembles", "erreurs", "demandes"),
  demande = c("groupe", "but", "parametres", "ensembles", "resultat", "erreurs", "effet", "effet_detail", "depuis"),
  parametre = c("type", "requis", "description", "valeurs", "defaut", "min", "max", "depuis"),
  champ = c("type", "description", "depuis"),
  resultat = c("type", "description", "champs"),
  type = c("type", "description", "champs"),
  erreur = c("message", "quand", "statut"))
problemes <- character(0)
signaler <- function(...) problemes <<- c(problemes, paste0(...))
texte <- function(x) is.character(x) && length(x) == 1 && nzchar(trimws(x))
cles_connues <- function(x, quelles, lieu) {
  en_trop <- setdiff(names(x), CLES[[quelles]])
  if (length(en_trop)) signaler(lieu, " : clés inconnues ", paste(en_trop, collapse = ", "))
}
objet <- function(x) is.list(x) && (!length(x) || !is.null(names(x)))

# Une version MAJEUR.MINEUR en nombres, pour les comparer.
en_nombres <- function(v) as.numeric(strsplit(v, ".", fixed = TRUE)[[1]])
avant_ou_egale <- function(a, b) { a <- en_nombres(a); b <- en_nombres(b); a[1] < b[1] || (a[1] == b[1] && a[2] <= b[2]) }
versions <- vapply(description$versions, function(v) as.character(v$version %||% ""), "")
for (v in versions) if (!grepl("^[0-9]+\\.[0-9]+$", v)) signaler("versions : « ", v, " » n'est pas MAJEUR.MINEUR")
if (length(versions) > 1 && all(grepl("^[0-9]+\\.[0-9]+$", versions)) &&
    !all(vapply(seq_along(versions)[-1], function(i) !avant_ou_egale(versions[i], versions[i - 1]), TRUE))) signaler("versions : pas dans l'ordre")
if (!identical(description$version, versions[length(versions)])) signaler("version : ", format(description$version), ", la dernière de versions est ", versions[length(versions)])
for (v in description$versions) if (!texte(v$changements)) signaler("versions : ", v$version, " sans changements")
depuis_valide <- function(x) is.null(x) || isTRUE(x %in% versions)

cles_connues(description, "racine", "la description")
types_nommes <- names(description$types)
# Une expression de type : des choix séparés par |, chacun un type de base ou un type nommé, suivi
# de [] autant de fois qu'il est un tableau.
type_valide <- function(t) texte(t) && all(vapply(strsplit(t, "|", fixed = TRUE)[[1]], function(x) {
  sub("(\\[\\])+$", "", trimws(x)) %in% c(TYPES_DE_BASE, types_nommes)
}, TRUE))
verifier_champs <- function(cs, lieu) {
  if (!objet(cs)) return(signaler(lieu, " : champs doit être un objet"))
  for (n in names(cs)) {
    c <- cs[[n]]
    l <- sprintf("%s, champ %s", lieu, n)
    cles_connues(c, "champ", l)
    if (!type_valide(c$type)) signaler(l, " : type invalide « ", format(c$type), " »")
    if (!texte(c$description)) signaler(l, " : description manquante")
    if (!depuis_valide(c$depuis)) signaler(l, " : depuis inconnu")
  }
}
verifier_parametres <- function(ps, lieu) {
  if (!objet(ps)) return(signaler(lieu, " : parametres doit être un objet"))
  for (n in names(ps)) {
    p <- ps[[n]]
    l <- sprintf("%s, paramètre %s", lieu, n)
    cles_connues(p, "parametre", l)
    if (!type_valide(p$type)) signaler(l, " : type invalide « ", format(p$type), " »")
    if (!is.logical(p$requis) || length(p$requis) != 1 || is.na(p$requis)) signaler(l, " : requis doit être true ou false")
    if (!texte(p$description)) signaler(l, " : description manquante")
    if (!is.null(p$valeurs) && (!objet(p$valeurs) || !length(p$valeurs) || !all(vapply(p$valeurs, is.character, TRUE))))
      signaler(l, " : valeurs doit être un objet { valeur: sens }")
    for (k in c("min", "max")) if (!is.null(p[[k]]) && !is.numeric(p[[k]])) signaler(l, " : ", k, " doit être un nombre")
    if (!depuis_valide(p$depuis)) signaler(l, " : depuis inconnu")
  }
}
for (n in types_nommes) {
  t <- description$types[[n]]
  cles_connues(t, "type", paste("type", n))
  if (!grepl("^[A-Z][A-Za-z]*$", n)) signaler("type ", n, " : un nom en PascalCase")
  if (!texte(t$description)) signaler("type ", n, " : description manquante")
  if (is.null(t$type) && is.null(t$champs)) signaler("type ", n, " : ni type ni champs")
  if (!is.null(t$type) && !type_valide(t$type)) signaler("type ", n, " : type invalide")
  if (!is.null(t$champs)) verifier_champs(t$champs, paste("type", n))
}
for (n in names(description$ensembles)) verifier_parametres(description$ensembles[[n]], paste("ensemble", n))
for (n in names(description$erreurs)) {
  e <- description$erreurs[[n]]
  cles_connues(e, "erreur", paste("erreur", n))
  if (!texte(e$message) || !texte(e$quand)) signaler("erreur ", n, " : message ou quand manquant")
}
groupes <- vapply(description$groupes, function(g) as.character(g$id), "")
for (n in decrites) {
  d <- description$demandes[[n]]
  lieu <- paste("demande", n)
  cles_connues(d, "demande", lieu)
  if (!isTRUE(d$groupe %in% groupes)) signaler(lieu, " : groupe inconnu ", format(d$groupe))
  # (une phrase : elle finit par un point, sans un autre point suivi d'une majuscule)
  if (!texte(d$but) || !grepl("\\.$", d$but) || grepl("\\. [A-Z]", d$but) || grepl("\n", d$but)) signaler(lieu, " : but doit être une phrase, finie par un point")
  verifier_parametres(d$parametres, lieu)
  for (e in d$ensembles) if (!isTRUE(e %in% names(description$ensembles))) signaler(lieu, " : ensemble inconnu ", e)
  if (!objet(d$resultat) || !type_valide(d$resultat$type)) signaler(lieu, " : resultat sans type valide")
  else {
    cles_connues(d$resultat, "resultat", paste(lieu, "résultat"))
    if (!is.null(d$resultat$champs)) verifier_champs(d$resultat$champs, paste(lieu, "résultat"))
  }
  if (!is.list(d$erreurs) || objet(d$erreurs) && length(d$erreurs)) signaler(lieu, " : erreurs doit être un tableau")
  for (e in d$erreurs) {
    if (is.character(e)) {
      if (!e %in% names(description$erreurs)) signaler(lieu, " : erreur commune inconnue ", e)
    } else if (!is.list(e) || !texte(e$message) || !texte(e$quand) || length(setdiff(names(e), c("message", "quand")))) {
      signaler(lieu, " : une erreur est le nom d'une erreur commune, ou { message, quand }")
    }
  }
  if (!isTRUE(d$effet %in% EFFETS)) signaler(lieu, " : effet inconnu ", format(d$effet))
  if (!is.null(d$effet_detail) && !texte(d$effet_detail)) signaler(lieu, " : effet_detail vide")
  if (!isTRUE(d$depuis %in% versions)) signaler(lieu, " : depuis inconnu ", format(d$depuis))
}
utilisees <- unique(unlist(lapply(description$demandes, function(d) Filter(is.character, d$erreurs))))
orphelines <- setdiff(names(description$erreurs), c(utilisees, "demande_inconnue", "jeton_refuse"))
if (length(orphelines)) signaler("erreurs communes citées par aucune demande : ", paste(orphelines, collapse = ", "))

cat("La description se tient\n")
verifier(sprintf("structure, types, erreurs, ensembles et versions (%d types, %d erreurs communes, version %s)",
                 length(types_nommes), length(description$erreurs), format(description$version)), !length(problemes), problemes)

# ---------------------------------------------------------------- ce qu'elles changent

# Une demande annulable garde un retour en arrière : sa fonction appelle memoriser, elle-même ou par
# les fonctions du moteur et du serveur qu'elle appelle, de proche en proche ; les autres non. Les
# fonctions qui y mènent : celles qui nomment memoriser, puis celles qui nomment l'une d'elles,
# jusqu'à ce qu'il ne s'en ajoute plus.
fonctions <- Filter(function(n) is.function(get(n, envir = globalenv())) && !is.primitive(get(n, envir = globalenv())), ls(globalenv()))
appelees <- function(f) intersect(all.names(body(f)), c(fonctions, "memoriser"))
appels <- lapply(stats::setNames(fonctions, fonctions), function(n) appelees(get(n, envir = globalenv())))
menent <- "memoriser"
repeat {
  nouvelles <- setdiff(names(Filter(function(a) any(a %in% menent), appels)), menent)
  if (!length(nouvelles)) break
  menent <- c(menent, nouvelles)
}
# (completer lie m et p dans la console, comme console, dont un projet affecté garde un retour en
# arrière ; la complétion, elle, n'évalue ni n'affecte rien)
SANS_AFFECTATION <- "completer"
ecarts <- character(0)
for (n in intersect(decrites, names(DEMANDES))) {
  annulable <- identical(description$demandes[[n]]$effet, "annulable")
  garde <- any(appelees(DEMANDES[[n]]) %in% menent) && !n %in% SANS_AFFECTATION
  if (annulable != garde) ecarts <- c(ecarts, sprintf("%s : décrite %s, %s", n, description$demandes[[n]]$effet,
                                                      if (garde) "garde un retour en arrière (memoriser)" else "ne garde pas de retour en arrière"))
}
cat("Ce qu'elles changent\n")
verifier(sprintf("« annulable » : les %d demandes qui gardent un retour en arrière, et elles seules",
                 sum(vapply(description$demandes, function(d) identical(d$effet, "annulable"), TRUE))), !length(ecarts), ecarts)

# ---------------------------------------------------------------- la demande protocole

cat("La demande protocole\n")
r <- DEMANDES$protocole(list())
paquet <- jsonlite::fromJSON(file.path(racine_nuee, "app", "package.json"))$version
verifier(sprintf("version %s, Nuée %s", format(r$version), format(r$nuee)), identical(r$version, description$version) && identical(r$nuee, paquet))
verifier("demandes : le nom de chacune, dans l'ordre", identical(as.character(r$demandes), sort(names(DEMANDES), method = "radix")) &&
           "protocole" %in% r$demandes && is.null(r$description))
# (telle que le client la reçoit : le JSON de la réponse, relu)
relue <- function(q) jsonlite::fromJSON(repondre(200L, DEMANDES$protocole(q))$body, simplifyVector = FALSE)
un <- relue(list(complet = FALSE))
verifier("en JSON, demandes reste un tableau", is.list(un$demandes) && length(un$demandes) == length(DEMANDES) && is.null(un$description))
complet <- relue(jsonlite::fromJSON('{"complet": true}', simplifyVector = TRUE))
verifier("avec complet, la description entière, telle que moteur/protocole.json la garde", identical(complet$description, description))

# ---------------------------------------------------------------- la page

cat("La page docs/protocole.md\n")
source(file.path(racine_nuee, "docs", "ecrire_protocole.R"), encoding = "UTF-8")
attendue <- page_protocole(description)
actuelle <- readLines(file.path(racine_nuee, "docs", "protocole.md"), encoding = "UTF-8", warn = FALSE)
differe <- which(c(attendue, rep("", max(0, length(actuelle) - length(attendue)))) != c(actuelle, rep("", max(0, length(attendue) - length(actuelle)))))
verifier(sprintf("à jour (%d lignes) : celle que docs/ecrire_protocole.R écrit de la description", length(attendue)), !length(differe),
         c(sprintf("elle diffère dès la ligne %d : Rscript docs/ecrire_protocole.R l'écrit à nouveau", differe[1])))

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
