#' Calculate median per year and stations 
#'
#' @param clean_minv Dataframe. Contains the biotic indices data
#' @param nb_analyses_par_annee Dataframe. 
#'
#' @return A list containing results in dataframes and plots
#' @export
#' 
#' @importFrom dplyr group_by summarise
#' @importFrom ggplot2 ggplot aes geom_col
#'
#' @examples
analyse_mediane_indices <- function(clean_minv, nb_analyses_par_annee) { 
  
  # Calcul de la médiane par année et par station
  med_par_annee <- clean_minv %>%
    group_by(libelle_indice, annee) %>%
    summarise(med = median(resultat_indice, na.rm = TRUE), .groups = "drop")
  
  #Médiane des indices par année avec le nombre de prélèvements
  ggplot() +
    geom_col(data = nb_analyses_par_annee, aes(x = annee, y = nb_analyses), fill = 'grey', alpha = 0.3) +
    geom_point(data = med_par_annee, aes(x = annee, y = med, color = as.factor(libelle_indice))) +
    geom_line(data = med_par_annee, aes(x = annee, y = med, color = as.factor(libelle_indice))) +
    facet_wrap(~libelle_indice, scales = "free_y") +
    labs(title = "Médiane des indices par année avec le nombre de prélèvements en arrière-plan",
         x = "Année",
         y = "Médiane de l'indice",
         color = "Indice") +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 0.8))
  
  #Médiane des indices par année
  ggplot(med_par_annee, aes(x = annee, y = med, color = as.factor(libelle_indice))) +
    geom_point() +
    geom_line() +
    facet_wrap(~libelle_indice, scales = "free_y") +
    labs(title = "Médiane des indices par année",
         x = "Année",
         y = "Médiane de l'indice",
         color = "Indice") +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 0.8))
  
  # Calcul de la médiane par station
  med_par_station <- clean_minv %>%
    group_by(libelle_indice, code_station_hydrobio) %>%
    summarise(med = median(resultat_indice, na.rm = TRUE), .groups = "drop")
  
  #Médiane des indices par station
  ggplot(med_par_station, aes(x = code_station_hydrobio, y = med, color = as.factor(libelle_indice))) +
    geom_col() +
    geom_line() +
    facet_wrap(~libelle_indice, scales = "free_y") +
    labs(title = "Médiane des indices par station",
         x = "Station",
         y = "Médiane de l'indice",
         color = "Indice") +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #Valeurs minimales en 2022 pour l'I2M2
  data_2022_i2m2 <- clean_minv %>%
    filter(libelle_indice == "Indice Invertébrés Multimétrique (I2M2)", annee == 2022)
  
  # Les 10 stations avec les valeurs les plus basses
  stations_min_i2m2_2022 <- data_2022_i2m2 %>%
    arrange(resultat_indice) %>%
    slice_head(n = 10) %>%
    select(libelle_station_hydrobio, code_station_hydrobio, resultat_indice)
  
  # Affichage des résultats
  print(stations_min_i2m2_2022)
  
  return(list(med_par_annee = med_par_annee, 
              med_par_station = med_par_station, 
              stations_min_i2m2_2022 = stations_min_i2m2_2022))
}


