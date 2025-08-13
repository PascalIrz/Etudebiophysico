library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ade4)
library(wordcloud) # Pour textplot
library(openxlsx)
library(purrr)

#Définir la fonction d'analyse RLQ
run_rlq_analysis <- function(selected_year,
                             path_to_traits = "C:/Users/ilona.garcia/Documents/RstudioGIT/Etudebiophysico/Data/traits_eco_bio.xlsx",
                             path_to_data = "Data/",
                             nutrients_param_codes = c("1339", "1340", "1433", "1335", "1350")) {
  
#Chargement des données brutes
load(file.path(path_to_data, "liste_taxon_ajout.Rdata"))
load(file.path(path_to_data, "df_taxo.rda"))
load(file.path(path_to_data, "physico_moyenne_annuelle.rda"))
traits <- read_excel(path = path_to_traits)
traits <- na.omit(traits)
traits <- traits %>%
filter(gr_bio %in% c("a", "b", "h", "e") | gr_eco %in% c("A", "D", "F", "C"))
  
  
#Créer un identifiant unique dans les deux jeux et filtrer par année ---
df_taxo_filtered <- abondance_relative %>%
  mutate(station_annee = paste(code_station_hydrobio, annee, sep = "_")) %>%
  filter(annee == selected_year)
  
df_physo_filtered <- physico_moyenne_annee %>%
  mutate(station_annee = paste(code_station_hydrobio, annee, sep = "_")) %>%
  filter(annee == selected_year)
  
#Vérifier si les données filtrées sont vides
if (nrow(df_taxo_filtered) == 0 || nrow(df_physo_filtered) == 0) {
  stop(paste("Aucune donnée disponible pour l'année", selected_year, ". Arrêt de l'analyse."))
}
  
#Ne conserver que les lignes de physico présentes en taxo
stations_taxo_uniques <- unique(df_taxo_filtered$station_annee)
  
df_physo_filtered <- df_physo_filtered %>%
  filter(station_annee %in% stations_taxo_uniques)
  
#Construction de la matrice R (Abondances) --- il y a eu confusion au niveau des noms, ça aurait du être la matrice L
R <- df_taxo_filtered %>%
  group_by(station_annee, Cd_Taxon_norm) %>%
  summarise(abondance_rel = sum(abondance_rel), .groups = "drop") %>%
  pivot_wider(
    names_from = Cd_Taxon_norm,
    values_from = abondance_rel,
    values_fill = 0
  ) %>%
  as.data.frame() %>%
  column_to_rownames("station_annee")
  
message(paste("Nombre de lignes dans la matrice d'abondance (R) pour", selected_year, ":", nrow(R)))
  
  
#Construction de la matrice L (Physico-chimique)
L <- df_physo_filtered %>%
  select(station_annee, code_parametre, para_moy_periode) %>%
  pivot_wider(
    names_from = code_parametre,
    values_from = para_moy_periode,
    values_fill = 0
  ) %>%
  as.data.frame() %>%
  column_to_rownames("station_annee")
  
message(paste("Nombre de lignes dans la matrice physico-chimique (L) pour", selected_year, ":", nrow(L)))
  
  
#Préparer la matrice Q (Traits)
taxon_map <- abondance_relative %>%
  select(Cd_Taxon_norm, Lb_Taxon) %>%
  distinct() %>%
  mutate(
    Cd_Taxon_norm = trimws(as.character(Cd_Taxon_norm)),
    Lb_Taxon = trimws(as.character(Lb_Taxon))
  )
  
Q_data <- traits %>%
  mutate(Lb_Taxon = trimws(as.character(Lb_Taxon))) %>%
  left_join(taxon_map, by = "Lb_Taxon") %>%
  filter(!is.na(Cd_Taxon_norm)) %>%
  distinct(Cd_Taxon_norm, .keep_all = TRUE) %>%
  as.data.frame() %>%
  column_to_rownames("Cd_Taxon_norm")
  
Q_final <- Q_data %>%
  select(-Lb_Taxon) %>%
  mutate(across(everything(), as.factor))
  
message(paste("Nombre de lignes dans la matrice de traits (Q) pour", selected_year, ":", nrow(Q_final)))
  
  
#Harmonisation finale des matrices R, L, Q
common_stations <- intersect(rownames(R), rownames(L))
R_final <- R[common_stations, , drop = FALSE]
L_final <- L[common_stations, , drop = FALSE]
  
if (!all(rownames(R_final) == rownames(L_final))) {
  stop("Erreur critique : les noms de lignes des matrices R (abondance) et L (physico) ne correspondent pas après harmonisation initiale.")
}
  
common_taxons <- intersect(colnames(R_final), rownames(Q_final))
R_final <- R_final[, common_taxons, drop = FALSE]
Q_final <- Q_final[common_taxons, , drop = FALSE]
  
if (!all(colnames(R_final) == rownames(Q_final))) {
  stop("Erreur critique : les noms de colonnes de la matrice R (abondance) et les noms de lignes de la matrice Q (traits) ne correspondent pas après harmonisation initiale.")
  }
  
#Filtration des lignes/colonnes avec variance ou somme nulle ---
R_final <- R_final[, colSums(R_final, na.rm = TRUE) > 0, drop = FALSE]
R_final <- R_final[rowSums(R_final, na.rm = TRUE) > 0, , drop = FALSE]
  
L_final <- L_final[, !sapply(L_final, function(x) var(x, na.rm = TRUE) == 0 || all(is.na(x))), drop = FALSE]
L_final <- L_final[rowSums(is.na(L_final)) < ncol(L_final), , drop = FALSE]
  
Q_final <- Q_final[, !sapply(Q_final, function(x) length(unique(x)) == 1), drop = FALSE]
Q_final <- Q_final[rowSums(is.na(Q_final)) < ncol(Q_final), , drop = FALSE]
  
#Ré-harmonisation ---
common_stations_final <- intersect(rownames(R_final), rownames(L_final))
R_final <- R_final[common_stations_final, , drop = FALSE]
L_final <- L_final[common_stations_final, , drop = FALSE]
  
common_taxons_final <- intersect(colnames(R_final), rownames(Q_final))
R_final <- R_final[, common_taxons_final, drop = FALSE]
Q_final <- Q_final[common_taxons_final, , drop = FALSE]
  
if (nrow(R_final) == 0 || ncol(R_final) == 0 || nrow(L_final) == 0 || ncol(L_final) == 0 || nrow(Q_final) == 0 || ncol(Q_final) == 0) {
  stop(paste("Les matrices R, L ou Q sont vides après harmonisation et filtrage pour l'année", selected_year, ". Arrêt de l'analyse."))
}
  
  message(paste("Dimensions finales pour", selected_year, ":"))
  message(paste("R (Abondance) :", paste(dim(R_final), collapse = "x")))
  message(paste("L (Physico) :", paste(dim(L_final), collapse = "x")))
  message(paste("Q (Traits) :", paste(dim(Q_final), collapse = "x")))
  
  # --- Analyse RLQ ---
  dudi_L_species <- dudi.coa(R_final, scannf = FALSE, nf = 2)
  dudi_R_env <- dudi.pca(L_final, scannf = FALSE, nf = 2)
  dudi_R_env$lw <- dudi_L_species$lw
  dudi_Q_traits <- dudi.hillsmith(Q_final, scannf = FALSE, nf = 2, row.w = dudi_L_species$cw)
  
  rlq_result <- rlq(dudi_R_env, dudi_L_species, dudi_Q_traits, scannf = FALSE, nf = 2)
  
  # --- Préparation des données pour le graphique combiné ---
  taxon_scores <- dudi_L_species$co
  env_weights <- rlq_result$l1
  trait_weights <- rlq_result$c1
  
  # Calculer le scaling_factor_vectors de manière robuste
  max_abs_taxon_score_x <- ifelse(length(taxon_scores[,1]) > 0, max(abs(taxon_scores[,1]), na.rm = TRUE), 1)
  max_abs_taxon_score_y <- ifelse(length(taxon_scores[,2]) > 0, max(abs(taxon_scores[,2]), na.rm = TRUE), 1)
  max_abs_weight <- ifelse(length(c(abs(env_weights), abs(trait_weights))) > 0, max(abs(env_weights), abs(trait_weights), na.rm = TRUE), 1)
  
  scaling_factor_vectors <- ifelse(max_abs_weight > 0, max(max_abs_taxon_score_x, max_abs_taxon_score_y) / max_abs_weight * 0.8, 1)
  
  
  # --- Création du graphique combiné ---
  plot_xlim_dynamic <- range(taxon_scores[,1], env_weights[,1] * scaling_factor_vectors, trait_weights[,1] * scaling_factor_vectors, na.rm = TRUE) * 1.1
  plot_ylim_dynamic <- range(taxon_scores[,2], env_weights[,2] * scaling_factor_vectors, trait_weights[,2] * scaling_factor_vectors, na.rm = TRUE) * 1.1
  
  png(file.path("Output", paste0("RLQ_Combined_Plot_", selected_year, ".png")), width = 1000, height = 800, res = 100)
  par(mar = c(5, 5, 4, 2) + 0.1)
  plot(taxon_scores, type = "n", asp = 1,
       xlim = plot_xlim_dynamic, ylim = plot_ylim_dynamic,
       xlab = paste0("Axe RLQ 1 (", round(rlq_result$eig[1] / sum(rlq_result$eig) * 100, 2), "%)"),
       ylab = paste0("Axe RLQ 2 (", round(rlq_result$eig[2] / sum(rlq_result$eig) * 100, 2), "%)"),
       main = paste0("Analyse RLQ : Taxons, paramètres physico-chimiques et traits (", selected_year, ")"))
  
  points(taxon_scores, pch = 20, col = "darkgreen", cex = 0.8)
  text(taxon_scores, labels = rownames(taxon_scores), cex = 0.6, pos = 3, col = "darkgreen")
  arrows(0, 0, env_weights[, 1] * scaling_factor_vectors, env_weights[, 2] * scaling_factor_vectors,
         length = 0.1, angle = 20, col = "darkblue", lwd = 1.5)
  text(env_weights[, 1] * scaling_factor_vectors * 1.1, env_weights[, 2] * scaling_factor_vectors * 1.1,
       labels = rownames(env_weights), cex = 0.7, col = "darkblue")
  arrows(0, 0, trait_weights[, 1] * scaling_factor_vectors, trait_weights[, 2] * scaling_factor_vectors,
         length = 0.1, angle = 20, col = "purple", lwd = 1.5)
  textplot(x = trait_weights[, 1] * scaling_factor_vectors * 1.1,
           y = trait_weights[, 2] * scaling_factor_vectors * 1.1,
           words = rownames(trait_weights),
           cex = 0.7, col = "purple",
           new = FALSE)
  legend("topleft",
         legend = c("Taxons", "Paramètres environnementaux", "Traits écologiques et biologiques"),
         pch = c(20, NA, NA), lty = c(NA, 1, 1), lwd = c(NA, 1.5, 1.5),
         col = c("darkgreen", "darkblue", "purple"), bty = "n", cex = 0.8)
  dev.off()
  
  
  # --- Extraction des scores et poids ---
  stations_scores_rlq <- dudi_L_species$li
  taxons_scores_rlq <- dudi_L_species$co
  env_parameters_weights_rlq <- rlq_result$l1
  traits_weights_rlq <- rlq_result$c1
  
  # --- Graphiques spécifiques (exemple pour un seul, mais peut être adapté) ---
  png(file.path("Output", paste0("RLQ_Stations_Plot_", selected_year, ".png")), width = 800, height = 700, res = 100)
  s.label(stations_scores_rlq, clab = 0.8,
          xlim = range(stations_scores_rlq[,1], na.rm = TRUE)*1.2, ylim = range(stations_scores_rlq[,2], na.rm = TRUE)*1.2)
  title(main = paste0("Projection des stations (RLQ) - ", selected_year))
  dev.off()
  
  # Exporter les scores et poids pour des analyses ultérieures
  write.xlsx(as.data.frame(stations_scores_rlq), file.path("Output", paste0("stations_scores_RLQ_", selected_year, ".xlsx")), rowNames = TRUE)
  write.xlsx(as.data.frame(taxons_scores_rlq), file.path("Output", paste0("taxons_scores_RLQ_", selected_year, ".xlsx")), rowNames = TRUE)
  write.xlsx(as.data.frame(env_parameters_weights_rlq), file.path("Output", paste0("env_weights_RLQ_", selected_year, ".xlsx")), rowNames = TRUE)
  write.xlsx(as.data.frame(traits_weights_rlq), file.path("Output", paste0("traits_weights_RLQ_", selected_year, ".xlsx")), rowNames = TRUE)
  
  # --- Analyse des seuils et export des listes ---
  seuil_bas_taxon <- quantile(taxons_scores_rlq[, 1], probs = 0.25, na.rm = TRUE)
  seuil_haut_taxon <- quantile(taxons_scores_rlq[, 1], probs = 0.75, na.rm = TRUE)
  seuil_bas_trait <- quantile(traits_weights_rlq[, 1], probs = 0.25, na.rm = TRUE)
  seuil_haut_trait <- quantile(traits_weights_rlq[, 1], probs = 0.75, na.rm = TRUE)
  seuil_bas_station <- quantile(stations_scores_rlq[, 1], probs = 0.25, na.rm = TRUE)
  seuil_haut_station <- quantile(stations_scores_rlq[, 1], probs = 0.75, na.rm = TRUE)
  
  # Initialisation des listes de taxons à retourner
  libelles_pression <- c()
  libelles_bonne_qualite <- c()
  
  # --- Taxons et traits associés à la "pression" ---
  taxons_pression <- taxons_scores_rlq[taxons_scores_rlq[, 1] < seuil_bas_taxon, , drop = FALSE]
  if (nrow(taxons_pression) > 0) {
    taxons_pression_sorted <- taxons_pression[order(taxons_pression[, 1], decreasing = TRUE), , drop = FALSE]
    codes_taxons_pression <- rownames(taxons_pression_sorted)
    libelles_pression <- unique(
      left_join(
        data.frame(code_taxon_net = tolower(trimws(codes_taxons_pression))),
        liste_taxon_ajout %>% mutate(code_taxon_net = tolower(trimws(Cd_Taxon_norm))) %>% select(code_taxon_net, Lb_Taxon),
        by = "code_taxon_net"
      )$Lb_Taxon
    )
    libelles_pression <- na.omit(libelles_pression) # S'assurer qu'il n'y a pas de NA
    write.xlsx(data.frame(LibelleTaxon = libelles_pression), file.path("Output", paste0("Taxons_associes_pressions_", selected_year, ".xlsx")), rowNames = FALSE)
  } else {
    message(paste("Aucun taxon associé à la pression pour", selected_year, "."))
  }
  
  traits_pression <- traits_weights_rlq[traits_weights_rlq[, 1] < seuil_bas_trait, , drop = FALSE]
  if (nrow(traits_pression) > 0) {
    traits_pression_sorted <- traits_pression[order(traits_pression[, 1], decreasing = TRUE), , drop = FALSE]
    write.xlsx(data.frame(Trait = rownames(traits_pression_sorted)), file.path("Output", paste0("Traits_associes_pressions_", selected_year, ".xlsx")), rowNames = FALSE)
  } else {
    message(paste("Aucun trait associé à la pression pour", selected_year, "."))
  }
  
  # --- Taxons et traits associés à la "bonne qualité" ---
  taxons_bonne_qualite <- taxons_scores_rlq[taxons_scores_rlq[, 1] > seuil_haut_taxon, , drop = FALSE]
  if (nrow(taxons_bonne_qualite) > 0) {
    taxons_bonne_qualite_sorted <- taxons_bonne_qualite[order(taxons_bonne_qualite[, 1], decreasing = FALSE), , drop = FALSE]
    codes_taxons_bonne_qualite <- rownames(taxons_bonne_qualite_sorted)
    libelles_bonne_qualite <- unique(
      left_join(
        data.frame(code_taxon_net = tolower(trimws(codes_taxons_bonne_qualite))),
        liste_taxon_ajout %>% mutate(code_taxon_net = tolower(trimws(Cd_Taxon_norm))) %>% select(code_taxon_net, Lb_Taxon),
        by = "code_taxon_net"
      )$Lb_Taxon
    )
    libelles_bonne_qualite <- na.omit(libelles_bonne_qualite) # S'assurer qu'il n'y a pas de NA
    write.xlsx(data.frame(LibelleTaxon = libelles_bonne_qualite), file.path("Output", paste0("Taxons_associes_bonne_qualite_", selected_year, ".xlsx")), rowNames = FALSE)
  } else {
    message(paste("Aucun taxon associé à la bonne qualité pour", selected_year, "."))
  }
  
  traits_bonne_qualite <- traits_weights_rlq[traits_weights_rlq[, 1] > seuil_haut_trait, , drop = FALSE]
  if (nrow(traits_bonne_qualite) > 0) {
    traits_bonne_qualite_sorted <- traits_bonne_qualite[order(traits_bonne_qualite[, 1], decreasing = FALSE), , drop = FALSE]
    write.xlsx(data.frame(Trait = rownames(traits_bonne_qualite_sorted)), file.path("Output", paste0("Traits_associes_bonne_qualite_", selected_year, ".xlsx")), rowNames = FALSE)
  } else {
    message(paste("Aucun trait associé à la bonne qualité pour", selected_year, "."))
  }
  
  # --- Identification des Stations, Taxons et Traits Associés aux Nutriments ---
  nutrients_weights <- env_parameters_weights_rlq[rownames(env_parameters_weights_rlq) %in% nutrients_param_codes, , drop = FALSE]
  
  if (nrow(nutrients_weights) > 0) {
    mean_nutrient_direction_Ax1 <- mean(nutrients_weights[, 1], na.rm = TRUE)
    mean_nutrient_direction_Ax2 <- mean(nutrients_weights[, 2], na.rm = TRUE)
    
    chosen_axis <- ifelse(abs(mean_nutrient_direction_Ax1) >= abs(mean_nutrient_direction_Ax2), 1, 2)
    message(paste0("L'Axe ", chosen_axis, " est choisi comme axe principal pour les nutriments pour l'année ", selected_year, "."))
    
    degraded_stations <- stations_scores_rlq[stations_scores_rlq[, chosen_axis] < seuil_bas_station, , drop = FALSE]
    if (nrow(degraded_stations) > 0) {
      degraded_stations_sorted <- degraded_stations[order(degraded_stations[, chosen_axis], decreasing = FALSE), , drop = FALSE]
      write.xlsx(as.data.frame(degraded_stations_sorted), file.path("Output", paste0("Stations_associes_nutriments_degrades_", selected_year, ".xlsx")), rowNames = TRUE)
    } else {
      message(paste("Aucune station dégradée par les nutriments trouvée pour", selected_year, "."))
    }
    
    taxons_degraded_conditions <- taxons_scores_rlq[taxons_scores_rlq[, chosen_axis] < seuil_bas_taxon, , drop = FALSE]
    if (nrow(taxons_degraded_conditions) > 0) {
      taxons_degraded_conditions_sorted <- taxons_degraded_conditions[order(taxons_degraded_conditions[, chosen_axis], decreasing = FALSE), , drop = FALSE]
      codes_taxons_degrades <- rownames(taxons_degraded_conditions_sorted)
      libelles_degrades <- unique(
        left_join(
          data.frame(code_taxon_net = tolower(trimws(codes_taxons_degrades))),
          liste_taxon_ajout %>% mutate(code_taxon_net = tolower(trimws(Cd_Taxon_norm))) %>% select(code_taxon_net, Lb_Taxon),
          by = "code_taxon_net"
        )$Lb_Taxon
      )
      write.xlsx(data.frame(LibelleTaxon = na.omit(libelles_degrades)), file.path("Output", paste0("Taxons_associes_nutriments_degrades_", selected_year, ".xlsx")), rowNames = FALSE)
    } else {
      message(paste("Aucun taxon dégradé par les nutriments trouvé pour", selected_year, "."))
    }
    
    traits_degraded_conditions <- traits_weights_rlq[traits_weights_rlq[, chosen_axis] < seuil_bas_trait, , drop = FALSE]
    if (nrow(traits_degraded_conditions) > 0) {
      traits_degraded_conditions_sorted <- traits_degraded_conditions[order(traits_degraded_conditions[, chosen_axis], decreasing = FALSE), , drop = FALSE]
      write.xlsx(data.frame(Trait = rownames(traits_degraded_conditions_sorted)), file.path("Output", paste0("Traits_associes_nutriments_degrades_", selected_year, ".xlsx")), rowNames = FALSE)
    } else {
      message(paste("Aucun trait dégradé par les nutriments trouvé pour", selected_year, "."))
    }
    
    # --- Visualisation spécifique du gradient de nutriments ---
    png(file.path("Output", paste0("RLQ_Nutrients_Gradient_Plot_", selected_year, ".png")), width = 1200, height = 1000, res = 100)
    
    max_abs_score_station <- ifelse(length(stations_scores_rlq) > 0, max(abs(stations_scores_rlq), na.rm = TRUE), 1)
    max_abs_score_taxon <- ifelse(length(taxons_scores_rlq) > 0, max(abs(taxons_scores_rlq), na.rm = TRUE), 1)
    max_abs_weight_combined <- ifelse(length(c(abs(env_parameters_weights_rlq), abs(traits_weights_rlq))) > 0, max(abs(env_parameters_weights_rlq), abs(traits_weights_rlq), na.rm = TRUE), 1)
    
    scaling_factor_viz <- max(max_abs_score_station, max_abs_score_taxon) / max_abs_weight_combined * 0.8
    
    plot(stations_scores_rlq, type = "n", asp = 1,
         xlim = range(stations_scores_rlq[,1], taxons_scores_rlq[,1], env_parameters_weights_rlq[,1] * scaling_factor_viz, traits_weights_rlq[,1] * scaling_factor_viz, na.rm=TRUE)*1.1,
         ylim = range(stations_scores_rlq[,2], taxons_scores_rlq[,2], env_parameters_weights_rlq[,2] * scaling_factor_viz, traits_weights_rlq[,2] * scaling_factor_viz, na.rm=TRUE)*1.1,
         xlab = paste0("Axe RLQ 1 (", round(rlq_result$eig[1] / sum(rlq_result$eig) * 100, 2), "%)"),
         ylab = paste0("Axe RLQ 2 (", round(rlq_result$eig[2] / sum(rlq_result$eig) * 100, 2), "%)"),
         main = paste0("RLQ: Stations, Taxons, Traits & Nutriments (", selected_year, ")"))
    
    points(stations_scores_rlq, pch = 20, col = "blue", cex = 0.8)
    points(taxons_scores_rlq, pch = 3, col = "darkgreen", cex = 0.8)
    
    for (i in 1:nrow(env_parameters_weights_rlq)) {
      col_arrow <- "darkgrey"
      lwd_arrow <- 1
      cex_label <- 0.7
      if (rownames(env_parameters_weights_rlq)[i] %in% nutrients_param_codes) {
        col_arrow <- "darkred"
        lwd_arrow <- 2
        cex_label <- 0.9
      }
      arrows(0, 0, env_parameters_weights_rlq[i, 1] * scaling_factor_viz, env_parameters_weights_rlq[i, 2] * scaling_factor_viz,
             length = 0.1, angle = 20, col = col_arrow, lwd = lwd_arrow)
      text(env_parameters_weights_rlq[i, 1] * scaling_factor_viz * 1.1, env_parameters_weights_rlq[i, 2] * scaling_factor_viz * 1.1,
           labels = rownames(env_parameters_weights_rlq)[i], cex = cex_label, col = col_arrow)
    }
    
    arrows(0, 0, traits_weights_rlq[, 1] * scaling_factor_viz, traits_weights_rlq[, 2] * scaling_factor_viz,
           length = 0.1, angle = 20, col = "purple", lwd = 1.5)
    text(traits_weights_rlq[, 1] * scaling_factor_viz * 1.1, traits_weights_rlq[, 2] * scaling_factor_viz * 1.1,
         labels = rownames(traits_weights_rlq), cex = 0.7, col = "purple")
    
    legend("topleft",
           legend = c("Stations", "Taxons", "Nutriments (Surlignés)", "Autres Param. Env.", "Traits Écologiques"),
           pch = c(20, 3, NA, NA, NA), lty = c(NA, NA, 1, 1, 1), lwd = c(NA, NA, 2, 1, 1.5),
           col = c("blue", "darkgreen", "darkred", "darkgrey", "purple"), bty = "n", cex = 0.8)
    dev.off()
  } else {
    message(paste("Aucun paramètre nutritif trouvé dans les données environnementales pour", selected_year, ". La visualisation spécifique des nutriments n'a pas été générée."))
  }
  
  message(paste0("--- Analyse RLQ pour l'année ", selected_year, " terminée. ---"))
  
  # Retourner les résultats pour d'autres analyses (ex: tests stat)
  return(list(
    selected_year = selected_year,
    rlq_result = rlq_result,
    dudi_L_species = dudi_L_species,
    dudi_R_env = dudi_R_env,
    dudi_Q_traits = dudi_Q_traits,
    stations_scores = stations_scores_rlq,
    taxons_scores = taxons_scores_rlq,
    env_weights = env_parameters_weights_rlq,
    traits_weights = traits_weights_rlq,
    # NOUVEAU : Retourner les listes de libellés de taxons
    taxons_pression_libelles = libelles_pression,
    taxons_bonne_qualite_libelles = libelles_bonne_qualite
  ))
}

# --- Créer un dossier "Output" si non existant ---
if (!dir.exists("Output")) {
  dir.create("Output")
}

# --- Exécution pour plusieurs années ---

# Définissez les années que vous souhaitez analyser
années_a_analyser <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023) # Ajouter ou modifier les années

# Utilisez purrr::map pour appliquer la fonction à chaque année
# `map` retourne une liste. `safely` capture les erreurs sans arrêter l'exécution.
# `.f = ~run_rlq_analysis(.x)` est une fonction anonyme qui passe chaque année (.x) à votre fonction.
liste_resultats_rlq <- purrr::map(années_a_analyser, function(annee) {
  tryCatch({
    run_rlq_analysis(selected_year = annee)
  }, error = function(e) {
    message(paste("Erreur lors de l'analyse pour l'année", annee, ":", e$message))
    return(NULL) # Retourne NULL en cas d'erreur pour cette année
  })
})

# Nommer les éléments de la liste pour un accès plus facile
names(liste_resultats_rlq) <- as.character(années_a_analyser)

# Supprimer les entrées NULL si certaines années ont échoué
liste_resultats_rlq <- purrr::compact(liste_resultats_rlq)

message("\n--- Analyse de toutes les années terminée. ---")

# : Comparaison par paires des listes de taxons (pression/qualité) SANS BOUCLE FOR ---
message("\n--- Démarrage des comparaisons par paires des listes de taxons 'pression' et 'bonne qualité' ---")

# Collecter tous les libellés de taxons uniques sur l'ensemble des années pour la "population" totale
all_taxons_observed <- unique(unlist(
  map(liste_resultats_rlq, ~c(.x$taxons_pression_libelles, .x$taxons_bonne_qualite_libelles))
))
all_taxons_observed <- all_taxons_observed[!is.na(all_taxons_observed)] # Enlever les NA

if (length(all_taxons_observed) == 0) {
  message("Aucun taxon n'a été identifié comme 'pression' ou 'bonne qualité' sur l'ensemble des années. Impossible de faire des comparaisons.")
} else {
  
  available_years_for_comparison <- sort(as.numeric(names(liste_resultats_rlq)))
  
  if (length(available_years_for_comparison) < 2) {
    message("Moins de 2 années avec des résultats disponibles pour la comparaison. Aucune comparaison par paires ne sera effectuée.")
  } else {
    # Générer toutes les paires uniques d'années
    year_pairs_df <- as.data.frame(t(combn(available_years_for_comparison, 2)))
    colnames(year_pairs_df) <- c("Year1", "Year2")
    
    # Fonction pour calculer les statistiques et le test de Fisher pour une paire et une catégorie
    perform_fisher_test <- function(year1_data, year2_data, category_name, all_taxons) {
      list_year1 <- year1_data
      list_year2 <- year2_data
      
      # S'assurer que les listes ne sont pas NULL ou vides, sinon les transformer en listes vides
      if (is.null(list_year1)) list_year1 <- character(0)
      if (is.null(list_year2)) list_year2 <- character(0)
      
      common_taxons <- intersect(list_year1, list_year2)
      only_year1 <- setdiff(list_year1, list_year2)
      only_year2 <- setdiff(list_year2, list_year1)
      not_in_either <- length(all_taxons) - length(union(list_year1, list_year2))
      
      matrix_counts <- matrix(c(
        length(common_taxons), length(only_year2),
        length(only_year1), not_in_either
      ), nrow = 2, byrow = TRUE)
      
      p_value <- NA
      if (all(dim(matrix_counts) == c(2,2)) && all(matrix_counts >= 0) && sum(matrix_counts) > 0) {
        tryCatch({
          fisher_test_result <- fisher.test(matrix_counts)
          p_value <- fisher_test_result$p.value
        }, error = function(e) {
          message(paste0("  Erreur lors du test de Fisher pour ", category_name, " (", year1, " vs ", year2, "): ", e$message))
        })
      } else {
        message(paste0("  Attention: Tableau de contingence invalide pour ", category_name, " (", year1, " vs ", year2, "). P-value non calculée."))
      }
      
      data.frame(
        Category = category_name,
        Intersect_Count = length(common_taxons),
        Only_Year1_Count = length(only_year1),
        Only_Year2_Count = length(only_year2),
        Not_In_Either_Count = not_in_either,
        P_Value = p_value,
        stringsAsFactors = FALSE
      )
    }
    
    # Préparer une liste de toutes les comparaisons à faire en utilisant expand_grid de tidyr
    comparison_tasks <- year_pairs_df %>%
      mutate(dummy = TRUE) %>% # Créer une colonne dummy pour expand_grid
      expand_grid(Category = c("Pression", "Bonne Qualité")) %>%
      select(-dummy) # Supprimer la colonne dummy si elle n'est plus nécessaire
    
    comparisons_results_list <- purrr::map_df(1:nrow(comparison_tasks), function(i) {
      y1 <- as.character(comparison_tasks$Year1[i])
      y2 <- as.character(comparison_tasks$Year2[i])
      cat <- comparison_tasks$Category[i]
      
      list1 <- if (cat == "Pression") liste_resultats_rlq[[y1]]$taxons_pression_libelles else liste_resultats_rlq[[y1]]$taxons_bonne_qualite_libelles
      list2 <- if (cat == "Pression") liste_resultats_rlq[[y2]]$taxons_pression_libelles else liste_resultats_rlq[[y2]]$taxons_bonne_qualite_libelles
      
      result <- perform_fisher_test(list1, list2, cat, all_taxons_observed)
      result$Year1 <- as.numeric(y1)
      result$Year2 <- as.numeric(y2)
      return(result)
    })
    
    # Correction des p-values pour les comparaisons multiples (Méthode de Holm)
    # Ne corriger que les p-values qui ne sont pas NA
    valid_p_values_idx <- !is.na(comparisons_results_list$P_Value)
    if (sum(valid_p_values_idx) > 0) {
      adjusted_p_values <- p.adjust(comparisons_results_list$P_Value[valid_p_values_idx], method = "holm")
      
      # Mettre à jour les résultats dans le dataframe
      comparisons_results_list$Adjusted_P_Value[valid_p_values_idx] <- adjusted_p_values
      comparisons_results_list$Significance[valid_p_values_idx] <- ifelse(adjusted_p_values < 0.05, "Significatif", "Non Significatif")
    } else {
      message("Aucune p-value valide à ajuster.")
    }
    
    message("\n--- Résumé des comparaisons par paires des listes de taxons ---")
    print(comparisons_results_list)
    write.xlsx(comparisons_results_list, file.path("Output", "Pairwise_Taxon_List_Comparison.xlsx"), rowNames = FALSE)
    message(paste0("Les résultats des comparaisons par paires des listes de taxons ont été sauvegardés dans Output/Pairwise_Taxon_List_Comparison.xlsx"))
  }
}

# --- FIN DE LA NOUVELLE SECTION ---

# --- Exemples de tests statistiques entre les années ---)

# Supposons que vous voulez comparer les scores des taxons entre 2018 et 2019
if ("2018" %in% names(liste_resultats_rlq) && "2019" %in% names(liste_resultats_rlq)) {
  scores_taxons_2018 <- liste_resultats_rlq[["2018"]]$taxons_scores
  scores_taxons_2019 <- liste_resultats_rlq[["2019"]]$taxons_scores # Correction ici pour 2019
  
  # Pour un test robuste, vous devriez aligner les taxons communs et choisir un axe
  common_taxons_test <- intersect(rownames(scores_taxons_2018), rownames(scores_taxons_2019))
  
  if (length(common_taxons_test) > 0) {
    # Vérifier s'il y a suffisamment de données après l'alignement
    if (length(scores_taxons_2018[common_taxons_test, 1]) > 1 && length(scores_taxons_2019[common_taxons_test, 1]) > 1) {
      scores_2018_common_ax1 <- scores_taxons_2018[common_taxons_test, 1] # Axe 1
      scores_2019_common_ax1 <- scores_taxons_2019[common_taxons_test, 1] # Axe 1
      
      wilcox_test_result_ax1 <- tryCatch({
        wilcox.test(scores_2018_common_ax1, scores_2019_common_ax1)
      }, error = function(e) {
        message(paste("Erreur lors du test de Wilcoxon pour l'Axe 1:", e$message))
        return(NULL)
      })
      
      if (!is.null(wilcox_test_result_ax1)) {
        message("\n--- Résultat du test de Wilcoxon sur les scores de taxons (Axe 1) entre 2018 et 2019 ---")
        print(wilcox_test_result_ax1)
      }
      
      # Vous pouvez répéter pour l'Axe 2 si pertinent
      if (ncol(scores_taxons_2018) >= 2 && ncol(scores_taxons_2019) >= 2) {
        scores_2018_common_ax2 <- scores_taxons_2018[common_taxons_test, 2] # Axe 2
        scores_2019_common_ax2 <- scores_taxons_2019[common_taxons_test, 2] # Axe 2
        
        wilcox_test_result_ax2 <- tryCatch({
          wilcox.test(scores_2018_common_ax2, scores_2019_common_ax2)
        }, error = function(e) {
          message(paste("Erreur lors du test de Wilcoxon pour l'Axe 2:", e$message))
          return(NULL)
        })
        
        if (!is.null(wilcox_test_result_ax2)) {
          message("\n--- Résultat du test de Wilcoxon sur les scores de taxons (Axe 2) entre 2018 et 2019 ---")
          print(wilcox_test_result_ax2)
        }
      }
      
    } else {
      message("Pas assez de taxons communs pour effectuer un test statistique entre 2018 et 2019.")
    }
  } else {
    message("Aucun taxon commun entre 2018 et 2019 pour le test statistique.")
  }
}

# --- Autre exemple : Comparaison des valeurs propres (variance expliquée) ---
message("\n--- Inertie expliquée par les deux premiers axes RLQ pour chaque année ---")
purrr::iwalk(liste_resultats_rlq, ~{
  if (!is.null(.x$rlq_result)) {
    total_eig <- sum(.x$rlq_result$eig)
    eig_perc_ax1 <- round(.x$rlq_result$eig[1] / total_eig * 100, 2)
    eig_perc_ax2 <- round(.x$rlq_result$eig[2] / total_eig * 100, 2)
    message(paste0("Année ", .y, ": Axe 1 = ", eig_perc_ax1, "%, Axe 2 = ", eig_perc_ax2, "%"))
  } else {
    message(paste0("Année ", .y, ": Résultats RLQ non disponibles."))
  }
})