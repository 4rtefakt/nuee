# Langue des messages que l'interface montre (erreurs, notes) : celle de l'interface, que le
# serveur reçoit avec chaque demande (option nuee.langue, "fr" ou "en"). Sans elle, le français :
# charger.R charge le moteur ainsi, comme avant la 1.0, pour les scripts qui lisent ses mots ;
# load.R le charge en anglais (nuee.langue = "en", sauf si on l'a réglée avant).
# traduire("texte en français", "English text") rend le texte dans cette langue.
traduire <- function(fr, en) if (identical(getOption("nuee.langue"), "en")) en else fr
