load("Data/classe_i2m2.rda")
load("Data/df_taxo.rda") 
library(tidyverse)
library(ggplot2)

# Nom du dataframe contenant les classifications I2M2 par station et année
df_i2m2_classification <- i2m2test

# Noms des colonnes dans df_i2m2_classification
nom_colonne_station_i2m2 <- "code_station_hydrobio"
nom_colonne_annee_i2m2 <- "annee"
nom_colonne_classe_i2m2 <- "classe_etat"

# Nom du dataframe contenant les abondances relatives (long format)
df_relative_abundance <- abondance_relative

# Noms des colonnes dans df_relative_abundance
nom_colonne_station_abondance <- "code_station_hydrobio"
nom_colonne_annee_abondance <- "annee"
nom_colonne_taxon_abondance <- "Cd_Taxon_norm"
nom_colonne_valeur_abondance <- "abondance_rel"
nom_colonne_groupe_taxo <- "GroupTaxo" # Nom de la colonne des groupes taxonomiques

# Année choisie pour l'analyse
annee_choisie <- 2017

# Définir les catégories I2M2 pour chaque groupe
categories_bon_etat <- c("Très Bon", "Bon")
categories_mediocre_etat <- c("Médiocre", "Mauvais")

# --- Filtrage initial des dataframes pour l'année choisie ---
df_i2m2_classification_filtered_year <- df_i2m2_classification[
  df_i2m2_classification[[nom_colonne_annee_i2m2]] == annee_choisie,
  , drop = FALSE
]
message(paste("Observations I2M2 pour l'année", annee_choisie, ":", nrow(df_i2m2_classification_filtered_year)))

df_relative_abundance_filtered_year <- df_relative_abundance[
  df_relative_abundance[[nom_colonne_annee_abondance]] == annee_choisie,
  , drop = FALSE
]
message(paste("Observations d'abondance pour l'année", annee_choisie, ":", nrow(df_relative_abundance_filtered_year)))


# --- Définition de la fonction d'analyse ---

# Cette fonction prend le type de filtrage (par codes taxon ou par groupe taxo)
# et les valeurs correspondantes.
run_analysis_for_taxa <- function(
    filter_by = c("taxon_codes", "group_names"), # "taxon_codes" ou "group_names"
    filter_values,
    analysis_name = "Taxon" 
) {
  
  # --- Filtrage des abondances relatives selon le type de filtre ---
  if (filter_by == "taxon_codes") {
    selected_taxa_relative_abundance_filtered <- df_relative_abundance_filtered_year[
      df_relative_abundance_filtered_year[[nom_colonne_taxon_abondance]] %in% filter_values,
      , drop = FALSE
    ]
  } else if (filter_by == "group_names") {
    selected_taxa_relative_abundance_filtered <- df_relative_abundance_filtered_year[
      df_relative_abundance_filtered_year[[nom_colonne_groupe_taxo]] %in% filter_values,
      , drop = FALSE
    ]
  } else {
    stop("filter_by doit être 'taxon_codes' ou 'group_names'.")
  }
  
  # Vérification si des données ont été trouvées
  if (nrow(selected_taxa_relative_abundance_filtered) == 0) {
    warning(paste("Aucune donnée pour", analysis_name, "trouvée avec les valeurs spécifiées pour l'année", annee_choisie, ". Passer à l'analyse suivante."))
    return(NULL) 
  }
  
  # Somme des abondances relatives pour chaque station-année
  summed_relative_abundance <- aggregate(
    selected_taxa_relative_abundance_filtered[[nom_colonne_valeur_abondance]],
    by = list(
      StationID = selected_taxa_relative_abundance_filtered[[nom_colonne_station_abondance]],
      Annee = selected_taxa_relative_abundance_filtered[[nom_colonne_annee_abondance]]
    ),
    FUN = sum
  )
  colnames(summed_relative_abundance)[3] <- paste0("total_relative_", tolower(gsub(" ", "_", analysis_name)))
  
  
  # Fusionner les abondances relatives avec les classifications I2M2
  data_for_analysis <- merge(
    summed_relative_abundance,
    df_i2m2_classification_filtered_year[, c(nom_colonne_station_i2m2, nom_colonne_annee_i2m2, nom_colonne_classe_i2m2)],
    by.x = c("StationID", "Annee"),
    by.y = c(nom_colonne_station_i2m2, nom_colonne_annee_i2m2),
    all.x = TRUE
  )
  
  # Supprimer les lignes où la classification I2M2 est manquante
  data_for_analysis <- na.omit(data_for_analysis)
  
  # Filtrer les observations en bon état et médiocre état
  observations_bon_etat_df <- data_for_analysis[
    data_for_analysis[[nom_colonne_classe_i2m2]] %in% categories_bon_etat,
    , drop = FALSE
  ]
  message(paste0("\nNombre d'observations (station-année) candidates 'bon état' pour ", analysis_name, " (", annee_choisie, ") : ", nrow(observations_bon_etat_df)))
  
  observations_mediocre_etat_df <- data_for_analysis[
    data_for_analysis[[nom_colonne_classe_i2m2]] %in% categories_mediocre_etat,
    , drop = FALSE
  ]
  message(paste0("Nombre d'observations (station-année) candidates 'médiocre état' pour ", analysis_name, " (", annee_choisie, ") : ", nrow(observations_mediocre_etat_df)))
  
  # --- Sélection d'un échantillon aléatoire de 10 observations ---
  set.seed(789) # Pour la reproductibilité
  
  if (nrow(observations_bon_etat_df) >= 10) {
    selected_observations_bon_etat <- observations_bon_etat_df[
      sample(nrow(observations_bon_etat_df), 10),
      , drop = FALSE
    ]
  } else {
    selected_observations_bon_etat <- observations_bon_etat_df
    warning(paste0("Moins de 10 observations 'bon état' disponibles pour ", analysis_name, " (", annee_choisie, "). Toutes les observations disponibles ont été sélectionnées."))
  }
  
  if (nrow(observations_mediocre_etat_df) >= 10) {
    selected_observations_mediocre_etat <- observations_mediocre_etat_df[
      sample(nrow(observations_mediocre_etat_df), 10),
      , drop = FALSE
    ]
  } else {
    selected_observations_mediocre_etat <- observations_mediocre_etat_df
    warning(paste0("Moins de 10 observations 'médiocre état' disponibles pour ", analysis_name, " (", annee_choisie, "). Toutes les observations disponibles ont été sélectionnées."))
  }
  
  message(paste0("\nObservations 'bon état' sélectionnées pour ", analysis_name, " (", annee_choisie, ") :"))
  print(selected_observations_bon_etat[, c("StationID", "Annee")])
  message(paste0("\nObservations 'médiocre état' sélectionnées pour ", analysis_name, " (", annee_choisie, ") :"))
  print(selected_observations_mediocre_etat[, c("StationID", "Annee")])
  
  # --- Extraire les abondances relatives ---
  # Le nom de la colonne d'abondance relative dépend du 'analysis_name'
  col_name_abundance <- paste0("total_relative_", tolower(gsub(" ", "_", analysis_name)))
  
  if (!col_name_abundance %in% colnames(selected_observations_bon_etat) ||
      !col_name_abundance %in% colnames(selected_observations_mediocre_etat)) {
    stop(paste("Colonne d'abondance relative", col_name_abundance, "introuvable dans les données sélectionnées."))
  }
  
  relative_bon_etat <- selected_observations_bon_etat[[col_name_abundance]]
  relative_mediocre_etat <- selected_observations_mediocre_etat[[col_name_abundance]]
  
  message(paste0("\nAbondance relative TOTALE des ", analysis_name, " dans les observations 'bon état' pour ", annee_choisie, " :"))
  print(relative_bon_etat)
  message(paste0("\nAbondance relative TOTALE des ", analysis_name, " dans les observations 'médiocre état' pour ", annee_choisie, " :"))
  print(relative_mediocre_etat)
  
  # --- Tester la signification de la différence ---
  all_relative_values <- c(relative_bon_etat, relative_mediocre_etat)
  group_labels_for_test <- factor(c(rep("Bon état", length(relative_bon_etat)),
                                    rep("Médiocre état", length(relative_mediocre_etat))))
  
  if (length(unique(group_labels_for_test)) < 2 || min(table(group_labels_for_test)) < 1) {
    warning(paste0("Pas assez d'observations dans les deux groupes pour le test de Wilcoxon pour ", analysis_name, ". Skipping test."))
    wilcox_test_result <- list(p.value = NA)
  } else {
    wilcox_test_result <- tryCatch({
      wilcox.test(all_relative_values ~ group_labels_for_test)
    }, error = function(e) {
      warning(paste0("Erreur lors de l'exécution du test de Wilcoxon pour ", analysis_name, ": ", e$message))
      return(list(p.value = NA))
    })
  }
  
  message(paste0("\nRésultat du test de Wilcoxon pour la différence d'abondance relative des ", analysis_name, " (Année ", annee_choisie, ") :"))
  print(wilcox_test_result)
  
  # Visualisation (Boxplot)
  boxplot(all_relative_values ~ group_labels_for_test,
          main = paste0("Abondance relative des ", analysis_name, " par classe I2M2 (Année ", annee_choisie, ")."),
          xlab = "Classe I2M2 (Observation station-année)",
          ylab = paste0("Abondance relative des ", analysis_name),
          col = c("lightgreen", "lightcoral"))
  
  return(list(
    wilcox_test_result = wilcox_test_result,
    relative_bon_etat = relative_bon_etat,
    relative_mediocre_etat = relative_mediocre_etat
  ))
}

# --- Exécution de la fonction pour les différents groupes ---


# Pour les Gammares
liste_codes_gammares <- c("888") 
resultat_gammares <- run_analysis_for_taxa(
  filter_by = "taxon_codes",
  filter_values = liste_codes_gammares,
  analysis_name = "Echinogammarus"
)

# Pour les EPT
groupes_ept <- c("Ephemoptera", "Plecoptera", "Trichoptera")
message("\n=== Analyse pour les EPT ===")
resultat_ept <- run_analysis_for_taxa(
  filter_by = "group_names",
  filter_values = groupes_ept,
  analysis_name = "EPT"
)


liste_codes_oligochetes <- c("933") 
resultat_oligochetes <- run_analysis_for_taxa(
filter_by = "taxon_codes",
filter_values = liste_codes_oligochetes,
analysis_name = "Oligochètes"
)

# Pour les Chironomes (si c'est un groupe taxonomique)

groupes_chironomes <- c("807")

resultat_chironomes <- run_analysis_for_taxa(
filter_by = "taxon_codes",
filter_values = groupes_chironomes,
analysis_name = "Chironomes"
)

#Aselles

groupes_aselles <- c("880")

resultat_aselles <- run_analysis_for_taxa(
  filter_by = "taxon_codes",
  filter_values = groupes_aselles,
  analysis_name = "Aselles"
)

groupes_baetis <- c("364")

resultat_baetis <- run_analysis_for_taxa(
  filter_by = "taxon_codes",
  filter_values = groupes_aselles,
  analysis_name = "Baetis"
)

#Potatruc --> pas significatif

groupes_pota <- c("978")

resultat_pota <- run_analysis_for_taxa(
  filter_by = "taxon_codes",
  filter_values = groupes_pota,
  analysis_name = "Potamopyrgus"
)

#Gold 

# Pour les GOLD
groupes_gold <- c("Gasteropoda", "Oligochaeta", "Diptera")
message("\n=== Analyse pour les GOLD ===")
resultat_gold <- run_analysis_for_taxa(
  filter_by = "group_names",
  filter_values = groupes_gold,
  analysis_name = "GOLD"
)

# Oligochete : différence significative

#Gammares non

#Baetis oui

#Chironomes non 
