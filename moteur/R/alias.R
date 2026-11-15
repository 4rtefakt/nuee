# Les noms anglais des fonctions que le guide montre pour se servir du moteur depuis R (load.R le
# charge en anglais) : les noms français restent et font la même chose.
# English names for the functions the guide shows for using the engine from R (load.R loads it in
# English): the French names stay and do the same.
import_table <- function(path, table = NULL, ...) importer_tableau(path, table = table, ...)
define_model <- function(project, type, x, y = NULL, class = NULL, observations = NULL, ...) {
  definir_modele(project, type, x, y = y, classe = class, observations = observations, ...)
}
fit_model <- function(project, definition, ...) ajuster(project, definition, ...)
