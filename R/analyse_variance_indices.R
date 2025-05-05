analyse_variance_indices <- function(clean_minv, seuil_top = 0.90) {
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(mapview)
  
  #Variance par année pour chaque indice
  var_par_annee <- clean_minv %>%
    group_by(libelle_indice, annee) %>%
    summarise(var = var(resultat_indice, na.rm = TRUE), .groups = "drop")
  
  plot_var_annee <- ggplot(var_par_annee, aes(x = annee, y = var, color = libelle_indice, group = libelle_indice)) +
    geom_point() + geom_line() +
    facet_wrap(~libelle_indice, scales = "free_y") +
    theme_minimal() + coord_cartesian(ylim = c(0, 0.25)) +
    labs(title = "Variance des indices au fil des années", x = "Année", y = "Variance")
  
  print(plot_var_annee)
  
  #Variance par station pour chaque indice
  var_par_station <- clean_minv %>%
    group_by(libelle_indice, code_station_hydrobio, libelle_station_hydrobio) %>%
    summarise(var = var(resultat_indice, na.rm = TRUE), .groups = "drop")
  
  plot_var_station <- ggplot(var_par_station, aes(x = code_station_hydrobio, y = var, fill = libelle_indice)) +
    geom_col(position = "dodge") +
    facet_wrap(~libelle_indice, scales = "free_y") +
    theme_minimal() + coord_cartesian(ylim = c(0, 0.15)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Variance des indices par station", x = "Station", y = "Variance")
  
  print(plot_var_station)
  
  #Calcul des quantiles des variances pour chaque indice
  quantiles_variance <- var_par_station %>%
    group_by(libelle_indice) %>%
    summarise(Q1 = quantile(var, 0.25, na.rm = TRUE),
              Mediane = quantile(var, 0.50, na.rm = TRUE),
              Q3 = quantile(var, 0.75, na.rm = TRUE),
              Seuil_top = quantile(var, seuil_top, na.rm = TRUE))
  
  var_par_station_quantiles <- var_par_station %>%
    inner_join(quantiles_variance, by = "libelle_indice")
  
  #️Sélection des stations les plus variables
  top10_var <- var_par_station_quantiles %>%
    filter(var >= Seuil_top)
  
  #Graphique des stations les plus variables
  ggplot(top10_var, aes(x = reorder(libelle_station_hydrobio, -var), y = var, fill = libelle_indice)) +
    geom_col() + facet_wrap(~libelle_indice, scales = "free_x") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Top", seuil_top * 100, "% des stations les plus variables par indice"),
         x = "Station", y = "Variance")
  
  #Stations présentes dans plusieurs indices
  stations_communes <- top10_var %>%
    group_by(libelle_station_hydrobio) %>%
    summarise(nb_indices = n(),
              libelle_indice = paste(unique(libelle_indice), collapse = ", ")) %>%
    filter(nb_indices >= 2)
  
  #Ajout des coordonnées géographiques
  stations_coords <- clean_minv %>%
    select(libelle_station_hydrobio, X = longitude, Y = latitude) %>%
    inner_join(stations_communes, by = "libelle_station_hydrobio")
  
  # Conversion en sf
  stations_sf <- st_as_sf(stations_coords, coords = c("X", "Y"), crs = 4326)
  
  cat("\n# Carte des stations les plus variables dans au moins 2 indices\n")
  print(mapview(stations_sf, zcol="nb_indices", col.regions=viridis::viridis, label=stations_sf$libelle_station_hydrobio))
  
  #I2M2
  top_var_i2m2 <- top10_var %>%
    filter(libelle_indice == "Indice Invertébrés Multimétrique (I2M2)")
  
  stations_coords_i2m2 <- clean_minv %>%
    filter(libelle_station_hydrobio %in% top_var_i2m2$libelle_station_hydrobio) %>%
    select(libelle_station_hydrobio, X = longitude, Y = latitude) %>%
    distinct()
  
  stations_sf_i2m2 <- st_as_sf(stations_coords_i2m2, coords = c("X", "Y"), crs = 4326)
  
  cat("\n# Carte des stations les plus variables pour l'I2M2\n")
  print(mapview(stations_sf_i2m2))
}

