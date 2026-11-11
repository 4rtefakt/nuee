# Langue des messages que l'interface montre (erreurs, notes) : celle de l'interface, que le
# serveur reçoit avec chaque demande (option nuee.langue, "fr" ou "en" ; "fr" par défaut).
# traduire("texte en français", "English text") rend le texte dans cette langue.
traduire <- function(fr, en) if (identical(getOption("nuee.langue"), "en")) en else fr
