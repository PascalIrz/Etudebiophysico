#' Moyenner des facteurs environnementaux sur des fenêtres glissantes de mois à mois
#'
#' @param .df Dataframe contenant les données. Contient obligatoirement les variables 
#'   concentration, mois, ainsi que des variables de groupage (ex : parametre, station).
#' @param .var_mois Nom de la variable indiquant le mois de l'observation.
#' @param .var_valeur Nom de la variable indiquant la valeur mensuelle de l'indicateur.
#' @param .mois_debut Entier entre 1 et 12. Mois de début de la fenêtre glissante.
#' @param .mois_fin Entier entre 1 et 12. Mois de fin de la fenêtre glissante. Doit être 
#'   supérieur ou égal à .mois_debut.
#' @param ... Nom des variables de groupage.
#'
#' @return Dataframe contenant les moyennes des paramètres par période.
#' @export
#' 
#' @importFrom dplyr group_by filter summarise mutate
#'
#' @examples
calculer_indicateur <- function(.df, .var_mois, .var_valeur, .mois_debut, .mois_fin, ...) {
  
  .df %>% 
    group_by(...) %>% 
    filter({{ .var_mois }} >= .mois_debut,
           {{ .var_mois }} <= .mois_fin) %>% 
    summarise(moy = mean({{ .var_valeur }}, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(debut = .mois_debut,
           fin = .mois_fin)
  
}

