# Le protocole du moteur (docs/protocole.md) : la demande `protocole`, qui donne sa version, celle de
# Nuée et le nom des demandes, et avec q$complet leur description (moteur/protocole.json : le but
# de chaque demande, ses paramètres, son résultat, ses erreurs, ce qu'elle change, la version depuis
# laquelle elle existe). Lu par serveur.R une fois ses DEMANDES toutes faites ;
# moteur/tests/verifier_protocole.R vérifie que chaque demande y est décrite, et rien d'autre.
#
# La version est MAJEUR.MINEUR : le mineur augmente quand une demande, un paramètre ou un champ du
# résultat s'ajoute (un client écrit pour 2.0 marche avec toute 2.x qui suit) ; le majeur, quand une
# demande disparaît ou change de nom, ou qu'un paramètre ou un champ change de sens ou disparaît.

# La description, lue à la première demande qui la veut (le démarrage du moteur n'attend pas).
memo_protocole <- new.env()
description_protocole <- function() {
  if (is.null(memo_protocole$description)) {
    memo_protocole$description <- jsonlite::fromJSON(file.path(racine, "moteur", "protocole.json"), simplifyVector = FALSE)
  }
  memo_protocole$description
}

# La version de Nuée : celle de l'application (app/package.json, à côté du moteur dans les sources
# comme dans une copie installée) ; NULL sans elle (le moteur seul). (Pas version_nuee, celle que
# greffons.R lit de NUEE_VERSION : lue après lui, elle la remplacerait.)
version_application <- function() {
  f <- file.path(racine, "app", "package.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(as.character(jsonlite::fromJSON(f)$version), error = function(e) NULL)
}

DEMANDES$protocole <- function(q) {
  description <- description_protocole()
  r <- list(version = description$version, nuee = version_application(),
            demandes = I(sort(names(DEMANDES), method = "radix")))  # (dans l'ordre des octets, quelle que soit la langue)
  if (isTRUE(q$complet)) r$description <- description
  r
}
