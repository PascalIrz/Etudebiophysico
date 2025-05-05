explorer_donnees <- function(clean_minv) { 
  library(dplyr)
  
  cat("\n### Exploration des données ###\n\n")
  
  # 1. Combien de stations ont un ou deux prélèvements par année (avant tri) ?
  cat("\n# Stations avec 1 ou 2 prélèvements par an (avant tri)\n")
  comptemultibis <- Multi_indice_minv %>%
    count(libelle_station_hydrobio, code_indice) %>%
    filter(n < 2, code_indice == 7613) %>%
    select("libelle_station_hydrobio")
  
  print(comptemultibis)
  
  # 2. Nombre total d'analyses par année
  cat("\n# Nombre total d'analyses par année\n")
  nb_analyses_par_annee <- clean_minv %>%
    group_by(annee) %>%
    summarise(nb_analyses = n(), .groups = "drop")
  
  print(nb_analyses_par_annee)
  
  # 3. Nombre d'années par station
  cat("\n# Nombre d'années par station\n")
  nb_annees_par_station <- clean_minv %>%
    group_by(code_station_hydrobio) %>%
    summarise(nb_annees = n_distinct(annee), .groups = "drop")
  
  print(nb_annees_par_station)
  
  # 4. Nombre d'analyses par année et station pour chaque indice
  cat("\n# Nombre d'analyses par année et station pour chaque indice\n")
  nb_analyses_par_annee_station <- clean_minv %>%
    group_by(code_station_hydrobio, annee, code_indice) %>%
    summarise(nb_analyses = n(), .groups = "drop") %>%
    mutate(multiple_analyses = if_else(nb_analyses > 1, "Oui", "Non"))
  
  # 5. Affichage des stations avec plus d'une analyse
  cat("\n# Stations avec plus d'une analyse par année\n")
  resultat <- nb_analyses_par_annee_station %>%
    filter(nb_analyses > 1)
  
  print(resultat)
  
  # 6. Vérification des doublons
  cat("\n# Vérification des doublons pour l'indice 7613\n")
  doublons <- Multi_indice_minv %>%
    count(code_station_hydrobio, code_indice, annee) %>%
    filter(n > 1 & code_indice == 7613)
  
  print(doublons)
  
  # 7. Résumé des valeurs clean_minv
  cat("\n# Résumé des valeurs\n")
  
  }
  
