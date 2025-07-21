# --- Charger les packages ---
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ade4)
library(wordcloud)
library(openxlsx)

# --- Chargement des données brutes ---
load("Data/abondance_diat.rda")        
load("Data/physico_moyenne_annuelle.rda") 
carayon <- read_excel(path ="C:/Users/ilona.garcia/Documents/RstudioGIT/Etudebiophysico/Data/carayon_diat.xlsx")
carayon <- na.omit(carayon)


# Créer un identifiant unique dans les deux jeux et filtrer par année ---
selected_year <- 2018

df_taxo_diat <- abondance_taxo_diat_filtre %>%
  mutate(station_annee = paste(code_station_hydrobio, annee, sep = "_")) %>%
  filter(annee == selected_year)

df_physo <- physico_moyenne_annee %>%
  mutate(station_annee = paste(code_station_hydrobio, annee, sep = "_")) %>%
  filter(annee == selected_year)

# --- Ne conserver que les lignes de physico présentes en taxo ---
stations_taxo_uniques_diat <- unique(df_taxo_diat$station_annee)

df_physo <- df_physo %>%
  filter(station_annee %in% stations_taxo_uniques_diat)

# --- Construction de la matrice R (Abondances) ---
R_diat <- df_taxo_diat %>%
  group_by(station_annee, code_appel_taxon) %>%
  summarise(abondance_rel = sum(abondance_rel), .groups = "drop") %>%
  pivot_wider(
    names_from = code_appel_taxon,
    values_from = abondance_rel,
    values_fill = 0
  ) %>%
  as.data.frame() %>%
  column_to_rownames("station_annee")

message(paste("Nombre de lignes dans la matrice d'abondance (R):", nrow(R)))


# --- Construction de la matrice L (Physico-chimique) ---

L_diat <- df_physo %>%
  select(station_annee, code_parametre, para_moy_periode) %>%
  pivot_wider(
    names_from = code_parametre,
    values_from = para_moy_periode,
    values_fill = 0
  ) %>%
  as.data.frame() %>%
  column_to_rownames("station_annee")

message(paste("Nombre de lignes dans la matrice physico-chimique (L):", nrow(L)))


# --- Préparer la matrice Q (Traits) ---
carayon_map <- abondance_taxo_diat_filtre %>%
  select(code_appel_taxon, libelle_appel_taxon) %>% 
  distinct() %>%
  mutate(
    code_appel_taxon = trimws(as.character(code_appel_taxon)),
    libelle_appel_taxon = trimws(as.character(libelle_appel_taxon)) 
  )

# Joindre les traits avec les codes taxons
Q_data_diat <- carayon %>%
  mutate(libelle_appel_taxon = trimws(as.character(libelle_taxon))) %>% # Nettoyer le libellé pour la jointure
  left_join(carayon_map, by = "libelle_appel_taxon") %>% 
  filter(!is.na(code_appel_taxon)) %>% # Exclure les taxons sans code correspondant
  distinct(code_appel_taxon, .keep_all = TRUE) %>%
  as.data.frame() %>%
  column_to_rownames("code_appel_taxon") # Utiliser le code taxon comme nom de ligne

# Nettoyage final de Q_data pour les traits (conversion en facteurs, suppression Lb_Taxon)
Q_final_diat <- Q_data_diat %>%
  select(-libelle_appel_taxon,-libelle_taxon, -`Total général`,-Taxon) %>%
  mutate(across(everything(), as.factor)) # Convertir toutes les colonnes de traits restantes en facteurs

message(paste("Nombre de lignes dans la matrice de traits (Q):", nrow(Q_final)))


# --- Harmonisation finale des matrices R, L, Q ---

# Harmonisation des stations (lignes de R et L)
common_stations_diat <- intersect(rownames(R_diat), rownames(L_diat))

# Filtrer les matrices pour ne garder que les stations communes et s'assurer de l'ordre
R_final_diat <- R_diat[common_stations_diat, , drop = FALSE]
L_final_diat <- L_diat[common_stations_diat, , drop = FALSE]

# Vérification (devrait être TRUE)
message(paste("Stations harmonisées :", all(rownames(R_final_diat) == rownames(L_final_diat))))
if (!all(rownames(R_final_diat) == rownames(L_final_diat))) {
  stop("Erreur critique : les noms de lignes des matrices R (abondance) et L (physico) ne correspondent pas après harmonisation initiale.")
}


# Harmonisation des taxons (colonnes de R et lignes de Q)
common_taxons_diat <- intersect(colnames(R_final_diat), rownames(Q_final_diat))

# Filtrer les matrices pour ne garder que les taxons communs et s'assurer de l'ordre
R_final_diat <- R_final_diat[, common_taxons_diat, drop = FALSE]
Q_final_diat <- Q_final_diat[common_taxons_diat, , drop = FALSE]

# Vérification (devrait être TRUE)
message(paste("Taxons harmonisés :", all(colnames(R_final_diat) == rownames(Q_final_diat))))
if (!all(colnames(R_final_diat) == rownames(Q_final_diat))) {
  stop("Erreur critique : les noms de colonnes de la matrice R (abondance) et les noms de lignes de la matrice Q (traits) ne correspondent pas après harmonisation initiale.")
}

# --- Filtration des lignes/colonnes avec variance ou somme nulle ---
R_final_diat <- R_final_diat[, colSums(R_final_diat, na.rm = TRUE) > 0, drop = FALSE] 
R_final_diat <- R_final_diat[rowSums(R_final_diat, na.rm = TRUE) > 0, , drop = FALSE]

L_final_diat <- L_final_diat[, !sapply(L_final_diat, function(x) var(x, na.rm = TRUE) == 0 || all(is.na(x))), drop = FALSE] # Supprime les paramètres constants/NA
L_final_diat <- L_final_diat[rowSums(is.na(L_final_diat)) < ncol(L_final_diat), , drop = FALSE] # Supprime les stations avec tous les NA

Q_final_diat <- Q_final_diat[, !sapply(Q_final_diat, function(x) length(unique(x)) == 1), drop = FALSE] # Supprime les traits constants
Q_final_diat <- Q_final_diat[rowSums(is.na(Q_final_diat)) < ncol(Q_final_diat), , drop = FALSE] # Supprime les taxons sans traits valides


# --- Ré-harmonisation ---

# Ré-harmonisation des stations entre R et L
common_stations_final_diat <- intersect(rownames(R_final_diat), rownames(L_final_diat))
R_final_diat <- R_final_diat[common_stations_final_diat, , drop = FALSE]
L_final_diat <- L_final_diat[common_stations_final_diat, , drop = FALSE]

# Ré-harmonisation des taxons entre R et Q
common_taxons_final_diat <- intersect(colnames(R_final_diat), rownames(Q_final_diat))
R_final_diat <- R_final_diat[, common_taxons_final_diat, drop = FALSE]
Q_final_diat <- Q_final_diat[common_taxons_final_diat, , drop = FALSE]

# Vérification finale des alignements
message(paste("Alignement R_final (lignes) et L_final (lignes) :", all(rownames(R_final_diat) == rownames(L_final_diat))))
message(paste("Alignement R_final (colonnes) et Q_final (lignes) :", all(colnames(R_final_diat) == rownames(Q_final_diat))))

if (!all(rownames(R_final_diat) == rownames(L_final_diat))) { stop("Erreur finale: R_final et L_final stations désalignées après filtrage.") }
if (!all(colnames(R_final_diat) == rownames(Q_final_diat))) { stop("Erreur finale: R_final et Q_final taxons désalignés après filtrage.") }

message(paste("R (Abondance) :", paste(dim(R_final_diat), collapse = "x")))
message(paste("L (Physico) :", paste(dim(L_final_diat), collapse = "x")))
message(paste("Q (Traits) :", paste(dim(Q_final_diat), collapse = "x")))


# dudi_L (Abondance) : dudi.coa
dudi_L_species_diat <- dudi.coa(R_final_diat, scannf = FALSE, nf = 2)

# Les poids des LIGNES (stations) doivent être les mêmes que ceux de dudi_L_species.
dudi_R_env_diat <- dudi.pca(L_final_diat, scannf = FALSE, nf = 2)
# Correction des poids des lignes APRÈS la création de l'objet dudi.pca.
dudi_R_env_diat$lw <- dudi_L_species_diat$lw

# dudi_Q (Traits) : dudi.hillsmith
dudi_Q_carayon_diat <- dudi.hillsmith(Q_final_diat, scannf = FALSE, nf = 2, row.w = dudi_L_species_diat$cw)


# --- RLQ ---
# L'ordre des arguments doit être: R (Environnement), L (Taxons), Q (Traits)
rlq_result_diat <- rlq(dudi_R_env_diat, dudi_L_species_diat, dudi_Q_carayon_diat, scannf = FALSE, nf = 2)

# --- Fourth-corner test ---
summary(rlq_result_diat)
plot(rlq_result_diat)



##############################################################################


# --- Préparation des données pour le graphique combiné ---

#TAXONS 
taxon_scores_diat <- dudi_L_species_diat$co

# PARAMÈTRES ENVIRONNEMENTAUX
env_weights_diat <- rlq_result_diat$l1

# TRAITS 
carayon_weights <- rlq_result_diat$c1


# Déterminer l'étendue maximale des SCORES des TAXONS
max_abs_taxon_score_x_diat <- max(abs(taxon_scores_diat[,1]))
max_abs_taxon_score_y_diat <- max(abs(taxon_scores_diat[,2]))

# Déterminer l'étendue maximale des POIDS (qui sont entre -1 et 1 si non scalés)
max_abs_weight_diat <- max(abs(env_weights_diat), abs(carayon_weights))

# Calculer un facteur de scaling pour les vecteurs (environnement et traits)
scaling_factor_vectors_diat <- max(max_abs_taxon_score_x_diat, max_abs_taxon_score_y_diat) / max_abs_weight_diat * 0.8

# Définir les limites globales pour le graphique en tenant compte des scores ET des vecteurs scalés
plot_xlim_diat <- c(min(taxon_scores_diat[,1], env_weights_diat[,1] * scaling_factor_vectors_diat, carayon_weights[,1] * scaling_factor_vectors_diat) * 1.1,
               max(taxon_scores_diat[,1], env_weights_diat[,1] * scaling_factor_vectors_diat, carayon_weights[,1] * scaling_factor_vectors_diat) * 1.1)

plot_ylim <- c(min(taxon_scores_diat[,2], env_weights_diat[,2] * scaling_factor_vectors_diat, carayon_weights[,2] * scaling_factor_vectors_diat) * 1.1,
               max(taxon_scores_diat[,2], env_weights_diat[,2] * scaling_factor_vectors_diat, carayon_weights[,2] * scaling_factor_vectors_diat) * 1.1)


# --- Création du graphique combiné ---
new_xlim <- c(-0.7,4)
new_ylim <- c(-4.8,8)

par(mar = c(5, 5, 4, 2) + 0.1)

plot(taxon_scores_diat, type = "n", asp = 1, 
     xlim = new_xlim, ylim = new_ylim,
     xlab = paste0("Axe RLQ 1 (", round(rlq_result_diat$eig[1]/sum(rlq_result_diat$eig)*100, 2), "%)"),
     ylab = paste0("Axe RLQ 2 (", round(rlq_result_diat$eig[2]/sum(rlq_result_diat$eig)*100, 2), "%)"),
     main = "Analyse RLQ : Taxons, paramètres physico-chimiques et traits")

# Ajouter les taxons
points(taxon_scores_diat, pch = 20, col = "darkgreen", cex = 0.8)
text(taxon_scores_diat, labels = rownames(taxon_scores_diat), cex = 0.6, pos = 3, col = "darkgreen") 

# Ajouter les paramètres environnementaux
arrows(0, 0, env_weights_diat[,1] * scaling_factor_vectors_diat, env_weights_diat[,2] * scaling_factor_vectors_diat,
       length = 0.1, angle = 20, col = "darkblue", lwd = 1.5)
text(env_weights_diat[,1] * scaling_factor_vectors_diat * 1.1, env_weights_diat[,2] * scaling_factor_vectors_diat * 1.1,
     
     labels = rownames(env_weights_diat), cex = 0.7, col = "darkblue") 

# Ajouter les traits
arrows(0, 0, carayon_weights[,1] * scaling_factor_vectors_diat, carayon_weights[,2] * scaling_factor_vectors_diat,
       length = 0.1, angle = 20, col = "purple", lwd = 1.5)
textplot(x = carayon_weights[,1] * scaling_factor_vectors_diat * 1.1,
         y = carayon_weights[,2] * scaling_factor_vectors_diat * 1.1,
         words = rownames(carayon_weights),
         cex = 0.7, col = "purple",
         new = FALSE)

# Légende
legend("topleft",
       legend = c("Taxons", "paramètres environnementaux", "traits écologiques et biologiques"),
       pch = c(20, NA, NA),
       lty = c(NA, 1, 1),
       lwd = c(NA, 1.5, 1.5),
       col = c("darkgreen", "darkblue", "purple"),
       bty = "n", cex = 0.8)


#####################################################################################################

# Scores des stations (sites) sur les axes RLQ (rlq_result$li)
stations_scores_rlq_diat <- dudi_L_species_diat$li

# Scores des taxons sur les axes RLQ (dudi_L_species$co)
taxons_scores_rlq_diat <- dudi_L_species_diat$co

# Poids canoniques des variables environnementales (rlq_result$l1)
env_parameters_weights_rlq_diat <- rlq_result_diat$l1

# Poids canoniques des traits (rlq_result$c1)
traits_weights_rlq_diat <- rlq_result_diat$c1

# --- Refaire les graphiques spécifiques avec zoom ---

# Graphique des R row scores (stations paramètres physico)
s.label(stations_scores_rlq_diat, clab = 0.8,
        xlim = c(min(stations_scores_rlq_diat[,1])*1.2, max(stations_scores_rlq_diat[,1])*1.2), # Ajustez manuellement pour zoom
        ylim = c(min(stations_scores_rlq_diat[,2])*1.2, max(stations_scores_rlq_diat[,2])*1.2)
)
title(main = "Projection des stations (RLQ)")

# Graphique des Q row scores (taxons)
s.label(taxons_scores_rlq_diat, clab = 0.8,
        xlim = c(min(taxons_scores_rlq_diat[,1])*1.2, max(taxons_scores_rlq_diat[,1])*1.2),
        ylim = c(min(taxons_scores_rlq_diat[,2])*1.2, max(taxons_scores_rlq_diat[,2])*1.2)
)
title(main = "Projection des Taxons (RLQ)")


# Graphique des R Canonical weights (Paramètres physico-chimiques)
par(mar = c(5, 5, 4, 2) + 0.1)
plot(env_parameters_weights_rlq_diat, type = "p", asp = 1,
     xlim = c(-1, 1), ylim = c(-1, 1),
     xlab = "Axe RLQ 1", ylab = "Axe RLQ 2",
     main = "Poids Canoniques des Paramètres Environnementaux (RLQ)")
arrows(0, 0, env_parameters_weights_rlq_diat[,1], env_parameters_weights_rlq_diat[,2], length = 0.1, angle = 20)
text(env_parameters_weights_rlq_diat[,1]*1.1, env_parameters_weights_rlq_diat[,2]*1.1, labels = rownames(env_parameters_weights_rlq_diat), cex = 0.8, col = "blue")
abline(h = 0, v = 0, lty = 2, col = "gray")
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray")


# Graphique des Q Canonical weights (traits)
par(mar = c(5, 5, 4, 2) + 0.1)
plot(traits_weights_rlq_diat, type = "p", asp = 1,
     xlim = c(-1, 1), ylim = c(-2, 2),
     xlab = "Axe RLQ 1", ylab = "Axe RLQ 2",
     main = "Poids Canoniques des Traits (RLQ)")
arrows(0, 0, traits_weights_rlq_diat[,1], traits_weights_rlq_diat[,2], length = 0.1, angle = 20)
textplot(x = traits_weights_rlq_diat[,1], y = traits_weights_rlq_diat[,2],
         words = rownames(traits_weights_rlq_diat),
         cex = 0.8, col = "purple",
         new = FALSE)
abline(h = 0, v = 0, lty = 2, col = "gray")
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray")


# --- Graphiques spécifiques combinés ---

max_abs_taxon_score_x_diat <- max(abs(taxons_scores_rlq_diat[,1]))
max_abs_taxon_score_y_diat <- max(abs(taxons_scores_rlq_diat[,2]))
max_abs_weight_diat <- max(abs(env_parameters_weights_rlq_diat), abs(traits_weights_rlq_diat))
scaling_factor_vectors_diat <- max(max_abs_taxon_score_x_diat, max_abs_taxon_score_y_diat) / max_abs_weight_diat * 0.8 # Ajustez 0.8 si nécessaire


# taxon-trait (points = taxons, flèches = traits)
par(mar = c(5, 5, 4, 2) + 0.1)
plot(taxons_scores_rlq_diat, type = "n", asp = 1,
     xlim = c(min(taxons_scores_rlq_diat[,1], traits_weights_rlq_diat[,1] * scaling_factor_vectors_diat)*1.1,
              max(taxons_scores_rlq_diat[,1], traits_weights_rlq_diat[,1] * scaling_factor_vectors_diat)*1.1),
     ylim = c(min(taxons_scores_rlq_diat[,2], traits_weights_rlq_diat[,2] * scaling_factor_vectors_diat)*1.1,
              max(taxons_scores_rlq_diat[,2], traits_weights_rlq_diat[,2] * scaling_factor_vectors_diat)*1.1),
     xlab = paste0("Axe RLQ 1 (", round(rlq_result_diat$eig[1]/sum(rlq_result_diat$eig)*100, 2), "%)"),
     ylab = paste0("Axe RLQ 2 (", round(rlq_result_diat$eig[2]/sum(rlq_result_diat$eig)*100, 2), "%)"),
     main = "Relations Taxons - Traits Écologiques")

# Points pour les taxons
points(taxons_scores_rlq_diat, pch = 20, col = "darkgreen", cex = 0.8)
text(taxons_scores_rlq_diat, labels = rownames(taxons_scores_rlq_diat), cex = 0.6, pos = 3, col = "darkgreen")

# Vecteurs pour les traits
arrows(0, 0, traits_weights_rlq_diat[,1] * scaling_factor_vectors_diat, traits_weights_rlq_diat[,2] * scaling_factor_vectors_diat,
       length = 0.1, angle = 20, col = "purple", lwd = 1.5)
text(traits_weights_rlq_diat[,1] * scaling_factor_vectors_diat * 1.1, traits_weights_rlq_diat[,2] * scaling_factor_vectors_diat * 1.1,
     labels = rownames(traits_weights_rlq_diat), cex = 0.7, col = "purple")

legend("topleft", legend = c("Taxons", "Traits écologiques"),
       pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
       col = c("darkgreen", "purple"), bty = "n", cex = 0.8)


# Trait-paramètre physico (flèches = traits, flèches = paramètres physico)
scaling_factor_env <- 1

# Calculer les limites dynamiques du graphique en incluant toutes les coordonnées
# pour s'assurer que tous les éléments et leurs étiquettes rentrent.
all_x_coords_diat <- c(traits_weights_rlq_diat[,1], env_parameters_weights_rlq_diat[,1] * scaling_factor_env)
all_y_coords_diat <- c(traits_weights_rlq_diat[,2], env_parameters_weights_rlq_diat[,2] * scaling_factor_env)

# Ajouter une petite marge aux limites pour les étiquettes
x_margin <- 0.15 * (max(all_x_coords_diat) - min(all_x_coords_diat))
y_margin <- 0.15 * (max(all_y_coords_diat) - min(all_y_coords_diat))

plot_xlim <- c(min(all_x_coords_diat) - x_margin, max(all_x_coords_diat) + x_margin)
plot_ylim <- c(min(all_y_coords_diat) - y_margin, max(all_y_coords_diat) + y_margin)

# Assurer des limites symétriques autour de zéro pour une meilleure interprétation des axes RLQ
max_abs_lim <- max(abs(plot_xlim), abs(plot_ylim))
plot_xlim <- c(-max_abs_lim, max_abs_lim)
plot_ylim <- c(-max_abs_lim, max_abs_lim)

# --- Création du graphique combiné ---
par(mar = c(5, 5, 4, 2) + 0.1)

plot(NULL, type = "n", asp = 1,
     xlim = plot_xlim, ylim = plot_ylim,
     xlab = paste0("Axe RLQ 1 (", round(rlq_result_diat$eig[1]/sum(rlq_result_diat$eig)*100, 2), "%)"),
     ylab = paste0("Axe RLQ 2 (", round(rlq_result_diat$eig[2]/sum(rlq_result_diat$eig)*100, 2), "%)"),
     main = "Relations Traits Écologiques - Paramètres Environnementaux (Vue Améliorée)")

# Ajouter la croix centrale (axes de référence)
abline(h = 0, v = 0, lty = 2, col = "gray")

# Ajouter le cercle unitaire
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray", lty = 2)

# --- Ajouter les vecteurs de traits ---
arrows(0, 0, traits_weights_rlq_diat[,1], traits_weights_rlq_diat[,2],
       length = 0.1, angle = 20, col = "purple", lwd = 1.5)
textplot(x = traits_weights_rlq_diat[,1], y = traits_weights_rlq_diat[,2],
         words = rownames(traits_weights_rlq_diat),
         cex = 0.8, col = "purple",
         new = FALSE)

# --- Ajouter les vecteurs des paramètres environnementaux ---
arrows(0, 0, env_parameters_weights_rlq_diat[,1] * scaling_factor_env,
       env_parameters_weights_rlq_diat[,2] * scaling_factor_env,
       length = 0.1, angle = 20, col = "darkblue", lwd = 1.5)
text(env_parameters_weights_rlq_diat[,1]*1.1, env_parameters_weights_rlq_diat[,2]*1.1, 
     labels = rownames(env_parameters_weights_rlq_diat), cex = 0.8, col = "blue")

# --- Légende ---
legend("topleft",
       legend = c("Traits Écologiques", "Paramètres Environnementaux"),
       lty = c(1, 1), lwd = c(1.5, 1.5),
       col = c("purple", "darkblue"), bty = "n", cex = 0.8)


# Taxon-Paramètre Physico (points = taxons, flèches = paramètres physico)
par(mar = c(5, 5, 4, 2) + 0.1)
plot(taxons_scores_rlq_diat, type = "n", asp = 1,
     xlim = c(min(taxons_scores_rlq_diat[,1], env_parameters_weights_rlq_diat[,1] * scaling_factor_vectors_diat)*1.1,
              max(taxons_scores_rlq_diat[,1], env_parameters_weights_rlq_diat[,1] * scaling_factor_vectors_diat)*1.1),
     ylim = c(min(taxons_scores_rlq_diat[,2], env_parameters_weights_rlq_diat[,2] * scaling_factor_vectors_diat)*1.1,
              max(taxons_scores_rlq_diat[,2], env_parameters_weights_rlq_diat[,2] * scaling_factor_vectors_diat)*1.1),
     xlab = paste0("Axe RLQ 1 (", round(rlq_result_diat$eig[1]/sum(rlq_result_diat$eig)*100, 2), "%)"),
     ylab = paste0("Axe RLQ 2 (", round(rlq_result_diat$eig[2]/sum(rlq_result_diat$eig)*100, 2), "%)"),
     main = "Relations Taxons - Paramètres Environnementaux")

# Points pour les Taxons
points(taxons_scores_rlq_diat, pch = 20, col = "darkgreen", cex = 0.8)
text(taxons_scores_rlq_diat, labels = rownames(taxons_scores_rlq_diat), cex = 0.6, pos = 3, col = "darkgreen")

# Vecteurs pour les Paramètres Environnementaux
arrows(0, 0, env_weights_diat[,1] * scaling_factor_vectors_diat, env_weights_diat[,2] * scaling_factor_vectors_diat,
       length = 0.1, angle = 20, col = "darkblue", lwd = 1.5)
text(env_weights_diat[,1] * scaling_factor_vectors_diat * 1.1, env_weights_diat[,2] * scaling_factor_vectors_diat * 1.1,
     labels = rownames(env_weights_diat), cex = 0.7, col = "darkblue")

legend("topleft", legend = c("Taxons", "Paramètres Environnementaux"),
       pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
       col = c("darkgreen", "darkblue"), bty = "n", cex = 0.8)

##########################################################################"

# Calcul des seuils pour les taxons et les traits sur l'Axe 1
seuil_bas_taxon_diat <- quantile(taxon_scores_diat[,1], probs = 0.25, na.rm = TRUE) # Correction ici
seuil_haut_taxon_diat <- quantile(taxon_scores_diat[,1], probs = 0.75, na.rm = TRUE) # Correction ici

seuil_bas_trait_diat <- quantile(carayon_weights[, 1], probs = 0.25, na.rm = TRUE) # Correction ici
seuil_haut_trait_diat <- quantile(carayon_weights[,1], probs = 0.75, na.rm = TRUE) # Correction ici


# --- Extraire les taxons et traits associés à la "pression" ---

# Taxons
taxons_pression_diat <- taxon_scores_diat[taxon_scores_diat[, 1] < seuil_bas_taxon_diat, ] # Correction ici

if (nrow(taxons_pression_diat) > 0) {
  taxons_pression_sorted_diat <- taxons_pression_diat[order(taxons_pression_diat[,1], decreasing = TRUE),]
  print(taxons_pression_sorted_diat)
} else {
  print("Aucun taxon ne dépasse le seuil pour la pression sur cet axe.")
}

codes_taxons_affiches_diat <- rownames(taxons_pression_sorted_diat)

codes_taxons_nets_diat <- tolower(trimws(codes_taxons_affiches_diat))

df_codes_pour_jointure_diat <- data.frame(
  code_taxon_net_diat = codes_taxons_nets_diat
)

# Assurez-vous que 'liste_taxon_ajout' est bien chargé quelque part avant ce script
# Si 'liste_taxon_ajout' vient des diatomées, il devrait aussi avoir un suffixe _diat si ce n'est pas déjà le cas
# Par exemple: load("Data/liste_taxon_ajout_diat.rda")
# Ici, je pars du principe que 'liste_taxon_ajout' est un référentiel global ou déjà spécifique aux diatomées.
referentiel_nettoye_diat <- abondance_taxo_diat_filtre %>% # Renommé
  mutate(
    code_taxon_net_diat = tolower(trimws(code_appel_taxon))
  ) %>%
  select(code_taxon_net_diat, libelle_appel_taxon)

taxons_avec_libelles_diat <- dplyr::left_join( # Renommé
  df_codes_pour_jointure_diat, # Correction ici
  referentiel_nettoye_diat, # Correction ici
  by = "code_taxon_net_diat"
)

liste_libelles_diat <- unique(taxons_avec_libelles_diat$libelle_taxon) # Renommé

if (length(liste_libelles_diat) > 0) {
  liste_libelles_sans_na_diat <- na.omit(liste_libelles_diat) # Renommé
  if (length(liste_libelles_sans_na_diat) > 0) {
    print(liste_libelles_sans_na_diat)
  } else {
    print("Aucun libellé correspondant trouvé pour les taxons sélectionnés.")
  }
} else {
  print("Aucun taxon ne dépasse le seuil pour la pression sur cet axe.")
}

df_to_export_taxons_pression_diat <- data.frame(LibelleTaxon = liste_libelles_sans_na_diat) # Renommé
write.xlsx(df_to_export_taxons_pression_diat, file = "Taxons_associes_pressions_diat.xlsx", rowNames = FALSE) # Renommé fichier


# Traits
traits_pression_diat <- carayon_weights[carayon_weights[,1] < seuil_bas_trait_diat, ] # Correction ici
if (nrow(traits_pression_diat) > 0) {
  traits_pression_sorted_diat <- traits_pression_diat[order(traits_pression_diat[,1], decreasing = TRUE),] # Renommé
  print(traits_pression_sorted_diat)
} else {
  print("Aucun trait ne dépasse le seuil pour la pression sur cet axe.")
}

df_to_export_traits_pression_diat <- data.frame(Trait = rownames(traits_pression_sorted_diat)) # Renommé
write.xlsx(df_to_export_traits_pression_diat, file = "Traits_associes_pressions_diat.xlsx", rowNames = FALSE) # Renommé fichier


# --- Extraire les taxons et traits associés à la "Bonne Qualité" ---
  
# Taxons
taxons_bonne_qualite_diat <- taxon_scores_diat[taxon_scores_diat[, 1] > seuil_haut_taxon_diat, ] # Correction ici
if (nrow(taxons_bonne_qualite_diat) > 0) {
  taxons_bonne_qualite_sorted_diat <- taxons_bonne_qualite_diat[order(taxons_bonne_qualite_diat[,1], decreasing = FALSE),] # Renommé
  print(taxons_bonne_qualite_sorted_diat)
} else {
  print("Aucun taxon ne dépasse le seuil pour la bonne qualité sur cet axe.")
}

good_taxons_affiches_diat <- rownames(taxons_bonne_qualite_sorted_diat) # Renommé

good_taxons_nets_diat <- tolower(trimws(good_taxons_affiches_diat)) # Renommé

df_codes_pour_jointure_good_diat <- data.frame( # Renommé
  good_taxon_net = good_taxons_nets_diat
)

referentiel_nettoye_good_diat <- abondance_taxo_diat_filtre %>% # Renommé
  mutate(
    good_taxon_net = tolower(trimws(code_appel_taxon))
  ) %>%
  select(good_taxon_net,libelle_appel_taxon)

good_taxons_avec_libelles_diat <- dplyr::left_join( # Renommé
  df_codes_pour_jointure_good_diat, # Correction ici
  referentiel_nettoye_good_diat, # Correction ici
  by = "good_taxon_net"
)

liste_libelles_good_diat <- unique(good_taxons_avec_libelles_diat$libelle_taxon) # Renommé

if (length(liste_libelles_good_diat) > 0) {
  liste_libelles_sans_na_good_diat <- na.omit(liste_libelles_good_diat) # Renommé
  if (length(liste_libelles_sans_na_good_diat) > 0) {
    print(liste_libelles_sans_na_good_diat)
  } else {
    print("Aucun libellé correspondant trouvé pour les taxons sélectionnés.")
  }
} else {
  print("Aucun taxon ne dépasse le seuil pour la pression sur cet axe.") # Garde ce message générique
}

df_to_export_bqualite_diat <- data.frame(LibelleTaxon = liste_libelles_sans_na_good_diat) # Renommé
write.xlsx(df_to_export_bqualite_diat, file = "Taxons_associes_bonne_qualite_diat.xlsx", rowNames = FALSE) # Renommé fichier


# Traits
traits_bonne_qualite_diat <- carayon_weights_diat[carayon_weights_diat[,1] > seuil_haut_trait_diat, ] # Correction ici
if (nrow(traits_bonne_qualite_diat) > 0) {
  traits_bonne_qualite_sorted_diat <- traits_bonne_qualite_diat[order(traits_bonne_qualite_diat[,1], decreasing = FALSE),] # Renommé
  print(traits_bonne_qualite_sorted_diat)
} else {
  print("Aucun trait ne dépasse le seuil pour la bonne qualité sur cet axe.")
}

df_to_export_traits_bqualite_diat <- data.frame(Trait = rownames(traits_bonne_qualite_sorted_diat)) # Renommé
write.xlsx(df_to_export_traits_bqualite_diat, file = "Traits_associes_bonne_qualite_diat.xlsx", rowNames = FALSE) # Renommé fichier
  
  ############## Nutriments ####################

nutrients_names_diat <- c("1339", "1340", "1433","1335", "1350") # Renommé, remplacez par vos noms exacts!


# Extraire les poids des nutriments
nutrients_weights_diat <- env_weights_diat[nutrients_names_diat, ] # Correction ici
print(nutrients_weights_diat)

# Calculer la "direction moyenne" des nutriments sur chaque axe.
mean_nutrient_direction_Ax1_diat <- mean(nutrients_weights_diat[,1]) # Renommé
mean_nutrient_direction_Ax2_diat <- mean(nutrients_weights_diat[,2]) # Renommé


# --- Automatisation du choix de l'axe le plus pertinent pour les nutriments ---
if (abs(mean_nutrient_direction_Ax1_diat) >= abs(mean_nutrient_direction_Ax2_diat)) { # Correction ici
  chosen_axis_diat <- 1 # Renommé
  mean_nutrient_direction_diat <- mean_nutrient_direction_Ax1_diat # Renommé
  cat("L'Axe 1 est choisi comme axe principal de la dégradation par les nutriments (diatomées).\n")
} else {
  chosen_axis_diat <- 2 # Renommé
  mean_nutrient_direction_diat <- mean_nutrient_direction_Ax2_diat # Renommé
  cat("L'Axe 2 est choisi comme axe principal de la dégradation par les nutriments (diatomées).\n")
}


# Assurez-vous que 'stations_scores_rlq_diat' est bien défini quelque part avant ce script
# Par exemple: stations_scores_rlq_diat <- rlq_result_diat$li
seuil_bas_station_diat <- quantile(stations_scores_rlq_diat[, 1], probs = 0.25, na.rm = TRUE) # Renommé
seuil_haut_station_diat <- quantile(stations_scores_rlq_diat[,1], probs = 0.75, na.rm = TRUE) # Renommé


### Identification des Stations, Taxons et Traits Associés aux Nutriments

# Identifier les Stations dégradées par les nutriments ---

degraded_stations_diat <- stations_scores_rlq_diat[ # Renommé
  stations_scores_rlq_diat[,chosen_axis_diat] < 0 & # Correction ici
    stations_scores_rlq_diat[,chosen_axis_diat] < seuil_bas_station_diat, ] # Correction ici

# Tri par score croissant (les plus négatifs sont ceux qui sont les plus "bas" sur l'axe négatif)
degraded_stations_sorted_diat <- degraded_stations_diat[order(degraded_stations_diat[,chosen_axis_diat], decreasing = FALSE), ] # Renommé et correction ici
message_no_station_diat <- paste0("Aucune station n'a de score entre 0 et -", seuil_bas_station_diat, " sur l'Axe ", chosen_axis_diat, " (diatomées).") # Renommé et correction ici


if (nrow(degraded_stations_sorted_diat) > 0) {
  print(degraded_stations_sorted_diat)
} else {
  print(message_no_station_diat)
}

# --- Étape 4 : Identifier les Taxons associés aux nutriments ---
taxons_degraded_conditions_diat <- taxon_scores_diat[ # Renommé et correction ici
  taxon_scores_diat[,chosen_axis_diat] < 0 & # Correction ici
    taxon_scores_diat[,chosen_axis_diat] < seuil_bas_taxon_diat, ] # Correction ici
taxons_degraded_conditions_sorted_diat <- taxons_degraded_conditions_diat[order(taxons_degraded_conditions_diat[,chosen_axis_diat], decreasing = FALSE), ] # Renommé et correction ici
message_no_taxon_diat <- paste0("Aucun taxon n'a de score entre 0 et -", seuil_bas_taxon_diat, " sur l'Axe ", chosen_axis_diat, " (diatomées).") # Renommé et correction ici

if (nrow(taxons_degraded_conditions_sorted_diat) > 0) {
  print(taxons_degraded_conditions_sorted_diat)
} else {
  print(message_no_taxon_diat)
}

bad_taxons_affiches_diat <- rownames(taxons_degraded_conditions_sorted_diat) # Renommé

bad_taxons_nets_diat <- tolower(trimws(bad_taxons_affiches_diat)) # Renommé

df_codes_pour_jointure_bad_diat <- data.frame( # Renommé
  bad_taxon_net = bad_taxons_nets_diat
)

referentiel_nettoye_bad_diat <- abondance_taxo_diat_filtre %>% # Renommé
  mutate(
    bad_taxon_net = tolower(trimws(code_appel_taxon))
  ) %>%
  select(bad_taxon_net,libelle_appel_taxon)

bad_taxons_avec_libelles_diat <- dplyr::left_join( # Renommé
  df_codes_pour_jointure_bad_diat, # Correction ici
  referentiel_nettoye_bad_diat, # Correction ici
  by = "bad_taxon_net"
)

liste_libelles_bad_diat <- unique(bad_taxons_avec_libelles_diat$libelle_taxon) # Renommé

if (length(liste_libelles_bad_diat) > 0) {
  liste_libelles_sans_na_bad_diat <- na.omit(liste_libelles_bad_diat) # Renommé
  if (length(liste_libelles_sans_na_bad_diat) > 0) {
    print(liste_libelles_sans_na_bad_diat)
  } else {
    print("Aucun libellé correspondant trouvé pour les taxons sélectionnés (diatomées).")
  }
} else {
  print("Aucun taxon ne dépasse le seuil pour la pression sur cet axe (diatomées).")
}

df_to_export_bad_taxons_diat <- data.frame(LibelleTaxon = liste_libelles_sans_na_bad_diat) # Renommé
write.xlsx(df_to_export_bad_taxons_diat, file = "Taxons_associes_mauvaise_qualite_nutriments_diat.xlsx", rowNames = FALSE) # Renommé fichier


# --- Étape 5 : Identifier les Traits associés aux nutriments ---

traits_degraded_conditions_diat <- carayon_weights[ # Renommé et correction ici
  carayon_weights[,chosen_axis_diat] < 0 & # Correction ici
    carayon_weights[,chosen_axis_diat] < seuil_bas_trait_diat, ] # Correction ici
traits_degraded_conditions_sorted_diat <- traits_degraded_conditions_diat[order(traits_degraded_conditions_diat[,chosen_axis_diat], decreasing = FALSE), ] # Renommé et correction ici
message_no_trait_diat <- paste0("Aucun trait n'a de poids entre 0 et -", seuil_bas_trait_diat, " sur l'Axe ", chosen_axis_diat, " (diatomées).") # Renommé et correction ici

if (nrow(traits_degraded_conditions_sorted_diat) > 0) {
  print(traits_degraded_conditions_sorted_diat)
} else {
  print(message_no_trait_diat)
}

df_to_export_bad_traits_diat <- data.frame(Trait = rownames(traits_degraded_conditions_sorted_diat)) # Renommé
write.xlsx(df_to_export_bad_traits_diat, file = "Traits_associes_mauvaise_qualite_nutriments_diat.xlsx", rowNames = FALSE) # Renommé fichier

---
  
  # --- Visualisation spécifique du gradient de nutriments ---
  # Redéfinir le scaling factor si vous voulez ajuster la visibilité
  # Assurez-vous que 'stations_scores_rlq_diat' est bien défini
  max_abs_score_station_diat <- max(abs(stations_scores_rlq_diat[,1]), abs(stations_scores_rlq_diat[,2])) # Renommé
max_abs_score_taxon_diat <- max(abs(taxon_scores_diat[,1]), abs(taxon_scores_diat[,2])) # Correction ici
max_abs_weight_combined_diat <- max(abs(env_weights_diat), abs(carayon_weights_diat)) # Renommé

# Scaling basé sur l'étendue des stations et taxons
# On veut que les vecteurs s'étendent bien par rapport aux points
scaling_factor_viz_diat <- max(max_abs_score_station_diat, max_abs_score_taxon_diat) / max_abs_weight_combined_diat * 0.8 # Renommé

par(mar = c(5, 5, 4, 2) + 0.1)
plot(stations_scores_rlq_diat, type = "n", asp = 1, # Renommé
     xlim = range(stations_scores_rlq_diat[,1], taxon_scores_diat[,1], env_weights_diat[,1] * scaling_factor_viz_diat, carayon_weights_diat[,1] * scaling_factor_viz_diat)*1.1, # Correction ici
     ylim = range(stations_scores_rlq_diat[,2], taxon_scores_diat[,2], env_weights_diat[,2] * scaling_factor_viz_diat, carayon_weights_diat[,2] * scaling_factor_viz_diat)*1.1, # Correction ici
     xlab = paste0("Axe RLQ 1 (", round(rlq_result_diat$eig[1]/sum(rlq_result_diat$eig)*100, 2), "%)"), # Correction ici
     ylab = paste0("Axe RLQ 2 (", round(rlq_result_diat$eig[2]/sum(rlq_result_diat$eig)*100, 2), "%)"), # Correction ici
     main = "RLQ: Stations, Taxons, Traits & Nutriments (Diatomées)") # Titre mis à jour

# 1. Ajouter les Stations (points bleus)
points(stations_scores_rlq_diat, pch = 20, col = "blue", cex = 0.8) # Renommé
# Optionnel: Ajouter des labels seulement pour les stations "dégradées" si trop nombreux
# if (exists("degraded_stations_sorted_diat") && nrow(degraded_stations_sorted_diat) > 0) {
#   text(degraded_stations_sorted_diat, labels = rownames(degraded_stations_sorted_diat), cex = 0.7, pos = 3, col = "red")
# }


# 2. Ajouter les Taxons (points verts)
points(taxon_scores_diat, pch = 3, col = "darkgreen", cex = 0.8) # Correction ici
# Optionnel: Ajouter des labels seulement pour les taxons "dégradés" si trop nombreux
# if (exists("taxons_degraded_conditions_sorted_diat") && nrow(taxons_degraded_conditions_sorted_diat) > 0) {
#   text(taxons_degraded_conditions_sorted_diat, labels = rownames(taxons_degraded_conditions_sorted_diat), cex = 0.7, pos = 1, col = "red")
# }

# 3. Ajouter les Paramètres Environnementaux (vecteurs bleus foncés), en surlignant les nutriments
for (i in 1:nrow(env_weights_diat)) { # Correction ici
  col_arrow <- "darkgrey" # Couleur par défaut
  lwd_arrow <- 1
  cex_label <- 0.7
  if (rownames(env_weights_diat)[i] %in% nutrients_names_diat) { # Correction ici
    col_arrow <- "darkred" # Couleur spécifique pour les nutriments
    lwd_arrow <- 2
    cex_label <- 0.9 # Label plus grand pour les nutriments
  }
  arrows(0, 0, env_weights_diat[i,1] * scaling_factor_viz_diat, env_weights_diat[i,2] * scaling_factor_viz_diat, # Correction ici
         length = 0.1, angle = 20, col = col_arrow, lwd = lwd_arrow)
  text(env_weights_diat[i,1] * scaling_factor_viz_diat * 1.1, env_weights_diat[i,2] * scaling_factor_viz_diat * 1.1, # Correction ici
       labels = rownames(env_weights_diat)[i], cex = cex_label, col = col_arrow) # Correction ici
}

# 4. Ajouter les Traits (vecteurs violets)
arrows(0, 0, carayon_weights_diat[,1] * scaling_factor_viz_diat, carayon_weights_diat[,2] * scaling_factor_viz_diat, # Correction ici
       length = 0.1, angle = 20, col = "purple", lwd = 1.5)
text(carayon_weights_diat[,1] * scaling_factor_viz_diat * 1.1, carayon_weights_diat[,2] * scaling_factor_viz_diat * 1.1, # Correction ici
     labels = rownames(carayon_weights_diat), cex = 0.7, col = "purple") # Correction ici

# Ajouter une légende
legend("topleft",
       legend = c("Stations", "Taxons (Diatomées)", "Nutriments (Surlignés)", "Autres Param. Env.", "Traits Écologiques (Diatomées)"), # Légende ajustée
       pch = c(20, 3, NA, NA, NA),
       lty = c(NA, NA, 1, 1, 1),
       lwd = c(NA, NA, 2, 1, 1.5),
       col = c("blue", "darkgreen", "darkred", "darkgrey", "purple"),
       bty = "n", cex = 0.8)
