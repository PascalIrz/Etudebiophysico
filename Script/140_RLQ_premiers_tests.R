#Chargement des librairies et des données
library(readxl)
library(tidyverse)
library(tibble)
library(ade4)
library(wordcloud)
library(openxlsx)

load("Data/liste_taxon_ajout.Rdata") 
load("Data/df_taxo.rda")        
load("Data/physico_moyenne_annuelle.rda") 

traits <- read_excel(path ="C:/Users/ilona.garcia/Documents/RstudioGIT/Etudebiophysico/Data/traits_eco_bio.xlsx")
traits <- na.omit(traits)
traits <- traits %>% 
  filter(gr_bio %in% c("a","b","h","e") | gr_eco %in% c("A","D","F","C"))


# Créer un identifiant unique dans les deux jeux et filtrer par année
selected_year <- 2022

df_taxo <- abondance_relative %>%
  mutate(station_annee = paste(code_station_hydrobio, annee, sep = "_")) %>%
  filter(annee == selected_year)

df_physo <- physico_moyenne_annee %>%
  mutate(station_annee = paste(code_station_hydrobio, annee, sep = "_")) %>%
  filter(annee == selected_year)

# Ne conserver que les lignes de physico présentes en taxo
stations_taxo_uniques <- unique(df_taxo$station_annee)

df_physo <- df_physo %>%
  filter(station_annee %in% stations_taxo_uniques)

# Construction de la matrice R (Abondances) - en principe ça aurait dû être la matrice L
R <- df_taxo %>%
  group_by(station_annee, Cd_Taxon_norm) %>%
  summarise(abondance_rel = sum(abondance_rel), .groups = "drop") %>%
  pivot_wider(
    names_from = Cd_Taxon_norm,
    values_from = abondance_rel,
    values_fill = 0
  ) %>%
  as.data.frame() %>%
  column_to_rownames("station_annee")

message(paste("Nombre de lignes dans la matrice d'abondance (R):", nrow(R)))


# Construction de la matrice L (Physico-chimique) - ça aurait du être R

L <- df_physo %>%
  select(station_annee, code_parametre, para_moy_periode) %>%
  pivot_wider(
    names_from = code_parametre,
    values_from = para_moy_periode,
    values_fill = 0
  ) %>%
  as.data.frame() %>%
  column_to_rownames("station_annee")

message(paste("Nombre de lignes dans la matrice physico-chimique (L):", nrow(L)))


# Préparer la matrice Q (Traits)
taxon_map <- abondance_relative %>%
  select(Cd_Taxon_norm, Lb_Taxon) %>% 
  distinct() %>%
  mutate(
    Cd_Taxon_norm = trimws(as.character(Cd_Taxon_norm)),
    Lb_Taxon = trimws(as.character(Lb_Taxon)) 
  )

# Joindre les traits avec les codes taxons
Q_data <- traits %>%
  mutate(Lb_Taxon = trimws(as.character(Lb_Taxon))) %>% # Nettoyer le libellé pour la jointure
  left_join(taxon_map, by = "Lb_Taxon") %>% 
  filter(!is.na(Cd_Taxon_norm)) %>% # Exclure les taxons sans code correspondant
  distinct(Cd_Taxon_norm, .keep_all = TRUE) %>%
  as.data.frame() %>%
  column_to_rownames("Cd_Taxon_norm") # Utiliser le code taxon comme nom de ligne

# Nettoyage final de Q_data pour les traits (conversion en facteurs, suppression Lb_Taxon)
Q_final <- Q_data %>%
  select(-Lb_Taxon) %>%
  mutate(across(everything(), as.factor)) # Convertir toutes les colonnes de traits restantes en facteurs

message(paste("Nombre de lignes dans la matrice de traits (Q):", nrow(Q_final)))


##Harmonisation finale des matrices R, L, Q 

# Harmonisation des stations (lignes de R et L)
common_stations <- intersect(rownames(R), rownames(L))

# Filtrer les matrices pour ne garder que les stations communes et s'assurer de l'ordre
R_final <- R[common_stations, , drop = FALSE]
L_final <- L[common_stations, , drop = FALSE]

# Vérification (devrait être TRUE)
message(paste("Stations harmonisées :", all(rownames(R_final) == rownames(L_final))))
if (!all(rownames(R_final) == rownames(L_final))) {
  stop("Erreur critique : les noms de lignes des matrices R (abondance) et L (physico) ne correspondent pas après harmonisation initiale.")
}


# Harmonisation des taxons (colonnes de R et lignes de Q)
common_taxons <- intersect(colnames(R_final), rownames(Q_final))

# Filtrer les matrices pour ne garder que les taxons communs et s'assurer de l'ordre
R_final <- R_final[, common_taxons, drop = FALSE]
Q_final <- Q_final[common_taxons, , drop = FALSE]

# Vérification (devrait être TRUE)
message(paste("Taxons harmonisés :", all(colnames(R_final) == rownames(Q_final))))
if (!all(colnames(R_final) == rownames(Q_final))) {
  stop("Erreur critique : les noms de colonnes de la matrice R (abondance) et les noms de lignes de la matrice Q (traits) ne correspondent pas après harmonisation initiale.")
}

#Filtration des lignes/colonnes avec variance ou somme nulle
R_final <- R_final[, colSums(R_final, na.rm = TRUE) > 0, drop = FALSE] 
R_final <- R_final[rowSums(R_final, na.rm = TRUE) > 0, , drop = FALSE]

L_final <- L_final[, !sapply(L_final, function(x) var(x, na.rm = TRUE) == 0 || all(is.na(x))), drop = FALSE] # Supprime les paramètres constants/NA
L_final <- L_final[rowSums(is.na(L_final)) < ncol(L_final), , drop = FALSE] # Supprime les stations avec tous les NA

Q_final <- Q_final[, !sapply(Q_final, function(x) length(unique(x)) == 1), drop = FALSE] # Supprime les traits constants
Q_final <- Q_final[rowSums(is.na(Q_final)) < ncol(Q_final), , drop = FALSE] # Supprime les taxons sans traits valides


##Ré-harmonisation

# Ré-harmonisation des stations entre R et L
common_stations_final <- intersect(rownames(R_final), rownames(L_final))
R_final <- R_final[common_stations_final, , drop = FALSE]
L_final <- L_final[common_stations_final, , drop = FALSE]

# Ré-harmonisation des taxons entre R et Q
common_taxons_final <- intersect(colnames(R_final), rownames(Q_final))
R_final <- R_final[, common_taxons_final, drop = FALSE]
Q_final <- Q_final[common_taxons_final, , drop = FALSE]

# Vérification finale des alignements
message(paste("Alignement R_final (lignes) et L_final (lignes) :", all(rownames(R_final) == rownames(L_final))))
message(paste("Alignement R_final (colonnes) et Q_final (lignes) :", all(colnames(R_final) == rownames(Q_final))))

if (!all(rownames(R_final) == rownames(L_final))) { stop("Erreur finale: R_final et L_final stations désalignées après filtrage.") }
if (!all(colnames(R_final) == rownames(Q_final))) { stop("Erreur finale: R_final et Q_final taxons désalignés après filtrage.") }

message(paste("R (Abondance) :", paste(dim(R_final), collapse = "x")))
message(paste("L (Physico) :", paste(dim(L_final), collapse = "x")))
message(paste("Q (Traits) :", paste(dim(Q_final), collapse = "x")))


# dudi_L (Abondance) : dudi.coa
dudi_L_species <- dudi.coa(R_final, scannf = FALSE, nf = 2)

# Les poids des LIGNES (stations) doivent être les mêmes que ceux de dudi_L_species.
dudi_R_env <- dudi.pca(L_final, scannf = FALSE, nf = 2)
# Correction des poids des lignes APRÈS la création de l'objet dudi.pca.
dudi_R_env$lw <- dudi_L_species$lw

# dudi_Q (Traits) : dudi.hillsmith
dudi_Q_traits <- dudi.hillsmith(Q_final, scannf = FALSE, nf = 2, row.w = dudi_L_species$cw)


# RLQ
# L'ordre des arguments doit être: R (Environnement), L (Taxons), Q (Traits)
rlq_result <- rlq(dudi_R_env, dudi_L_species, dudi_Q_traits, scannf = FALSE, nf = 2)

#Fourth-corner test
summary(rlq_result)
plot(rlq_result)



##############################################################################
#Préparation des données pour le graphique combiné

#TAXONS 
taxon_scores <- dudi_L_species$co

# PARAMÈTRES ENVIRONNEMENTAUX
env_weights <- rlq_result$l1

# TRAITS 
trait_weights <- rlq_result$c1


# Déterminer l'étendue maximale des SCORES des TAXONS
max_abs_taxon_score_x <- max(abs(taxon_scores[,1]))
max_abs_taxon_score_y <- max(abs(taxon_scores[,2]))

# Déterminer l'étendue maximale des POIDS
max_abs_weight <- max(abs(env_weights), abs(trait_weights))

# Calculer un facteur de scaling pour les vecteurs (environnement et traits)
scaling_factor_vectors <- max(max_abs_taxon_score_x, max_abs_taxon_score_y) / max_abs_weight * 0.8

# Définir les limites globales pour le graphique en tenant compte des scores ET des vecteurs scalés
plot_xlim <- c(min(taxon_scores[,1], env_weights[,1] * scaling_factor_vectors, trait_weights[,1] * scaling_factor_vectors) * 1.1,
               max(taxon_scores[,1], env_weights[,1] * scaling_factor_vectors, trait_weights[,1] * scaling_factor_vectors) * 1.1)

plot_ylim <- c(min(taxon_scores[,2], env_weights[,2] * scaling_factor_vectors, trait_weights[,2] * scaling_factor_vectors) * 1.1,
               max(taxon_scores[,2], env_weights[,2] * scaling_factor_vectors, trait_weights[,2] * scaling_factor_vectors) * 1.1)


#Création du graphique combiné
new_xlim <- c(-0.7,4)
new_ylim <- c(-4.8,8)

par(mar = c(5, 5, 4, 2) + 0.1)

plot(taxon_scores, type = "n", asp = 1, 
     xlim = new_xlim, ylim = new_ylim,
     xlab = paste0("Axe RLQ 1 (", round(rlq_result$eig[1]/sum(rlq_result$eig)*100, 2), "%)"),
     ylab = paste0("Axe RLQ 2 (", round(rlq_result$eig[2]/sum(rlq_result$eig)*100, 2), "%)"),
     main = "Analyse RLQ : Taxons, paramètres physico-chimiques et traits")

# Ajouter les taxons
points(taxon_scores, pch = 20, col = "darkgreen", cex = 0.8)
text(taxon_scores, labels = rownames(taxon_scores), cex = 0.6, pos = 3, col = "darkgreen") 

# Ajouter les paramètres environnementaux
arrows(0, 0, env_weights[,1] * scaling_factor_vectors, env_weights[,2] * scaling_factor_vectors,
       length = 0.1, angle = 20, col = "darkblue", lwd = 1.5)
text(env_weights[,1] * scaling_factor_vectors * 1.1, env_weights[,2] * scaling_factor_vectors * 1.1,
     
     labels = rownames(env_weights), cex = 0.7, col = "darkblue") 

# Ajouter les traits
arrows(0, 0, trait_weights[,1] * scaling_factor_vectors, trait_weights[,2] * scaling_factor_vectors,
       length = 0.1, angle = 20, col = "purple", lwd = 1.5)
textplot(x = trait_weights[,1] * scaling_factor_vectors * 1.1,
         y = trait_weights[,2] * scaling_factor_vectors * 1.1,
         words = rownames(trait_weights),
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

# Scores des stations sur les axes RLQ
stations_scores_rlq <- dudi_L_species$li

# Scores des taxons sur les axes RLQ
taxons_scores_rlq <- dudi_L_species$co

# Poids canoniques des variables environnementales
env_parameters_weights_rlq <- rlq_result$l1

# Poids canoniques des traits
traits_weights_rlq <- rlq_result$c1

# Refaire les graphiques spécifiques avec zoom

# Graphique des R row scores
s.label(stations_scores_rlq, clab = 0.8,
        xlim = c(min(stations_scores_rlq[,1])*1.2, max(stations_scores_rlq[,1])*1.2), # Ajustez manuellement pour zoom
        ylim = c(min(stations_scores_rlq[,2])*1.2, max(stations_scores_rlq[,2])*1.2)
)
title(main = "Projection des stations (RLQ)")

# Graphique des Q row scores
s.label(taxons_scores_rlq, clab = 0.8,
        xlim = c(min(taxons_scores_rlq[,1])*1.2, max(taxons_scores_rlq[,1])*1.2),
        ylim = c(min(taxons_scores_rlq[,2])*1.2, max(taxons_scores_rlq[,2])*1.2)
)
title(main = "Projection des taxons (RLQ)")


# Graphique des L Canonical weights
par(mar = c(5, 5, 4, 2) + 0.1)
plot(env_parameters_weights_rlq, type = "p", asp = 1,
     xlim = c(-1, 1), ylim = c(-1, 1),
     xlab = "Axe RLQ 1", ylab = "Axe RLQ 2",
     main = "Poids canoniques des paramètres environnementaux (RLQ)")
arrows(0, 0, env_parameters_weights_rlq[,1], env_parameters_weights_rlq[,2], length = 0.1, angle = 20)
text(env_parameters_weights_rlq[,1]*1.1, env_parameters_weights_rlq[,2]*1.1, labels = rownames(env_parameters_weights_rlq), cex = 0.8, col = "blue")
abline(h = 0, v = 0, lty = 2, col = "gray")
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray")


# Graphique des Q Canonical weights
par(mar = c(5, 5, 4, 2) + 0.1)
plot(traits_weights_rlq, type = "p", asp = 1,
     xlim = c(-1, 1), ylim = c(-2, 2),
     xlab = "Axe RLQ 1", ylab = "Axe RLQ 2",
     main = "Poids canoniques des traits (RLQ)")
arrows(0, 0, traits_weights_rlq[,1], traits_weights_rlq[,2], length = 0.1, angle = 20)
textplot(x = traits_weights_rlq[,1], y = traits_weights_rlq[,2],
         words = rownames(traits_weights_rlq),
         cex = 0.8, col = "purple",
         new = FALSE)
abline(h = 0, v = 0, lty = 2, col = "gray")
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray")


#Graphiques combinés

max_abs_taxon_score_x <- max(abs(taxons_scores_rlq[,1]))
max_abs_taxon_score_y <- max(abs(taxons_scores_rlq[,2]))
max_abs_weight <- max(abs(env_parameters_weights_rlq), abs(traits_weights_rlq))
scaling_factor_vectors <- max(max_abs_taxon_score_x, max_abs_taxon_score_y) / max_abs_weight * 0.8 # Ajustez 0.8 si nécessaire


# taxon-trait (points = taxons, flèches = traits)
par(mar = c(5, 5, 4, 2) + 0.1)
plot(taxons_scores_rlq, type = "n", asp = 1,
     xlim = c(min(taxons_scores_rlq[,1], traits_weights_rlq[,1] * scaling_factor_vectors)*1.1,
              max(taxons_scores_rlq[,1], traits_weights_rlq[,1] * scaling_factor_vectors)*1.1),
     ylim = c(min(taxons_scores_rlq[,2], traits_weights_rlq[,2] * scaling_factor_vectors)*1.1,
              max(taxons_scores_rlq[,2], traits_weights_rlq[,2] * scaling_factor_vectors)*1.1),
     xlab = paste0("Axe RLQ 1 (", round(rlq_result$eig[1]/sum(rlq_result$eig)*100, 2), "%)"),
     ylab = paste0("Axe RLQ 2 (", round(rlq_result$eig[2]/sum(rlq_result$eig)*100, 2), "%)"),
     main = "Relations taxons - traits écologiques")

# Points pour les taxons
points(taxons_scores_rlq, pch = 20, col = "darkgreen", cex = 0.8)
text(taxons_scores_rlq, labels = rownames(taxons_scores_rlq), cex = 0.6, pos = 3, col = "darkgreen")

# Vecteurs pour les traits
arrows(0, 0, traits_weights_rlq[,1] * scaling_factor_vectors, traits_weights_rlq[,2] * scaling_factor_vectors,
       length = 0.1, angle = 20, col = "purple", lwd = 1.5)
text(traits_weights_rlq[,1] * scaling_factor_vectors * 1.1, traits_weights_rlq[,2] * scaling_factor_vectors * 1.1,
     labels = rownames(traits_weights_rlq), cex = 0.7, col = "purple")

legend("topleft", legend = c("Taxons", "Traits écologiques"),
       pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
       col = c("darkgreen", "purple"), bty = "n", cex = 0.8)


# Trait-paramètre physico (flèches = traits, flèches = paramètres physico)
scaling_factor_env <- 1

# Calculer les limites dynamiques du graphique en incluant toutes les coordonnées
# pour s'assurer que tous les éléments et leurs étiquettes rentrent.
all_x_coords <- c(traits_weights_rlq[,1], env_parameters_weights_rlq[,1] * scaling_factor_env)
all_y_coords <- c(traits_weights_rlq[,2], env_parameters_weights_rlq[,2] * scaling_factor_env)

# Ajouter une petite marge aux limites pour les étiquettes
x_margin <- 0.15 * (max(all_x_coords) - min(all_x_coords))
y_margin <- 0.15 * (max(all_y_coords) - min(all_y_coords))

plot_xlim <- c(min(all_x_coords) - x_margin, max(all_x_coords) + x_margin)
plot_ylim <- c(min(all_y_coords) - y_margin, max(all_y_coords) + y_margin)

# Assurer des limites symétriques autour de zéro pour une meilleure interprétation des axes RLQ
max_abs_lim <- max(abs(plot_xlim), abs(plot_ylim))
plot_xlim <- c(-max_abs_lim, max_abs_lim)
plot_ylim <- c(-max_abs_lim, max_abs_lim)

#Création du graphique combiné
par(mar = c(5, 5, 4, 2) + 0.1)

plot(NULL, type = "n", asp = 1,
     xlim = plot_xlim, ylim = plot_ylim,
     xlab = paste0("Axe RLQ 1 (", round(rlq_result$eig[1]/sum(rlq_result$eig)*100, 2), "%)"),
     ylab = paste0("Axe RLQ 2 (", round(rlq_result$eig[2]/sum(rlq_result$eig)*100, 2), "%)"),
     main = "Relations traits écologiques - Paramètres environnementaux")

# Ajouter la croix centrale (axes de référence)
abline(h = 0, v = 0, lty = 2, col = "gray")

# Ajouter le cercle unitaire
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray", lty = 2)

#Ajouter les vecteurs de traits
arrows(0, 0, traits_weights_rlq[,1], traits_weights_rlq[,2],
       length = 0.1, angle = 20, col = "purple", lwd = 1.5)
textplot(x = traits_weights_rlq[,1], y = traits_weights_rlq[,2],
         words = rownames(traits_weights_rlq),
         cex = 0.8, col = "purple",
         new = FALSE)

#Ajouter les vecteurs des paramètres environnementaux
arrows(0, 0, env_parameters_weights_rlq[,1] * scaling_factor_env,
       env_parameters_weights_rlq[,2] * scaling_factor_env,
       length = 0.1, angle = 20, col = "darkblue", lwd = 1.5)
text(env_parameters_weights_rlq[,1]*1.1, env_parameters_weights_rlq[,2]*1.1, 
     labels = rownames(env_parameters_weights_rlq), cex = 0.8, col = "blue")

# Légendes
legend("topleft",
       legend = c("Traits écologiques", "Paramètres environnementaux"),
       lty = c(1, 1), lwd = c(1.5, 1.5),
       col = c("purple", "darkblue"), bty = "n", cex = 0.8)


# Taxon-Paramètre physico
par(mar = c(5, 5, 4, 2) + 0.1)
plot(taxons_scores_rlq, type = "n", asp = 1,
     xlim = c(min(taxons_scores_rlq[,1], env_parameters_weights_rlq[,1] * scaling_factor_vectors)*1.1,
              max(taxons_scores_rlq[,1], env_parameters_weights_rlq[,1] * scaling_factor_vectors)*1.1),
     ylim = c(min(taxons_scores_rlq[,2], env_parameters_weights_rlq[,2] * scaling_factor_vectors)*1.1,
              max(taxons_scores_rlq[,2], env_parameters_weights_rlq[,2] * scaling_factor_vectors)*1.1),
     xlab = paste0("Axe RLQ 1 (", round(rlq_result$eig[1]/sum(rlq_result$eig)*100, 2), "%)"),
     ylab = paste0("Axe RLQ 2 (", round(rlq_result$eig[2]/sum(rlq_result$eig)*100, 2), "%)"),
     main = "Relations taxons - Paramètres environnementaux")

# Points pour les taxons
points(taxons_scores_rlq, pch = 20, col = "darkgreen", cex = 0.8)
text(taxons_scores_rlq, labels = rownames(taxons_scores_rlq), cex = 0.6, pos = 3, col = "darkgreen")

# Vecteurs pour les paramètres physico-chimiques
arrows(0, 0, env_parameters_weights_rlq[,1] * scaling_factor_vectors, env_parameters_weights_rlq[,2] * scaling_factor_vectors,
       length = 0.1, angle = 20, col = "darkblue", lwd = 1.5)
text(env_parameters_weights_rlq[,1] * scaling_factor_vectors * 1.1, env_parameters_weights_rlq[,2] * scaling_factor_vectors * 1.1,
     labels = rownames(env_parameters_weights_rlq), cex = 0.7, col = "darkblue")

legend("topleft", legend = c("Taxons", "Paramètres environnementaux"),
       pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
       col = c("darkgreen", "darkblue"), bty = "n", cex = 0.8)

##########################################################################"

# Méthode de seuillage
seuil_bas_taxon <- quantile(taxons_scores_rlq[,1], probs = 0.25, na.rm = TRUE)
seuil_haut_taxon <- quantile(taxons_scores_rlq[,1], probs = 0.75, na.rm = TRUE)

seuil_bas_trait <- quantile(traits_weights_rlq[, 1], probs = 0.25, na.rm = TRUE)
seuil_haut_trait <- quantile(traits_weights_rlq[,1], probs = 0.75, na.rm = TRUE)


#Extraire les taxons et traits associés à la "pression"

#Taxons
taxons_pression <- taxons_scores_rlq[taxons_scores_rlq[, 1] < seuil_bas_taxon, ]

if (nrow(taxons_pression) > 0) {
  taxons_pression_sorted <- taxons_pression[order(taxons_pression[,1], decreasing = TRUE),]
  print(taxons_pression_sorted)
} else {
  print("Aucun taxon ne dépasse le seuil pour la pression sur cet axe.")
}

codes_taxons_affiches <- rownames(taxons_pression_sorted)

codes_taxons_nets <- tolower(trimws(codes_taxons_affiches))

df_codes_pour_jointure <- data.frame(
  code_taxon_net = codes_taxons_nets
)

referentiel_nettoye <- liste_taxon_ajout %>%
  mutate(
    code_taxon_net = tolower(trimws(Cd_Taxon_norm))
  ) %>%
  select(code_taxon_net, Lb_Taxon) 

colnames(referentiel_nettoye)[colnames(referentiel_nettoye) == "Lb_Taxon"] <- "libelle_taxon"

taxons_avec_libelles <- dplyr::left_join(
  df_codes_pour_jointure,
  referentiel_nettoye,
  by = "code_taxon_net"
)

liste_libelles <- unique(taxons_avec_libelles$libelle_taxon)


if (length(liste_libelles) > 0) {
  liste_libelles_sans_na <- na.omit(liste_libelles)
  if (length(liste_libelles_sans_na) > 0) {
    print(liste_libelles_sans_na)
  } else {
    print("Aucun libellé correspondant trouvé pour les taxons sélectionnés.")
  }
} else {
  print("Aucun taxon ne dépasse le seuil pour la pression sur cet axe.")
}

df_to_export <- data.frame(LibelleTaxon = liste_libelles_sans_na)
write.xlsx(df_to_export, file = "Taxons_associes_pressions.xlsx", rowNames = FALSE)


# Traits

traits_pression <- traits_weights_rlq[traits_weights_rlq[,1] < seuil_bas_trait, ]
if (nrow(traits_pression) > 0) {
  traits_pression_sorted <- traits_pression[order(traits_pression[,1], decreasing = TRUE),]
  print(traits_pression_sorted)
} else {
  print("Aucun trait ne dépasse le seuil pour la pression sur cet axe.")
}

df_to_export_traits <- data.frame(Trait = rownames(traits_pression)) # <<< CORRECTION ICI
write.xlsx(df_to_export_traits, file = "Traits_associes_pressions.xlsx", rowNames = FALSE)

#Extraire les taxons et traits associés à la "Bonne qualité"

# Taxons
taxons_bonne_qualite <- taxons_scores_rlq[taxons_scores_rlq[, 1] > seuil_haut_taxon, ]
if (nrow(taxons_bonne_qualite) > 0) {
  taxons_bonne_qualite_sorted <- taxons_bonne_qualite[order(taxons_bonne_qualite[,1], decreasing = FALSE),]
  print(taxons_bonne_qualite_sorted)
} else {
  print("Aucun taxon ne dépasse le seuil pour la bonne qualité sur cet axe.")
}

good_taxons_affiches <- rownames(taxons_bonne_qualite_sorted)

good_taxons_nets <- tolower(trimws(good_taxons_affiches))

df_codes_pour_jointure_good <- data.frame(
  good_taxon_net = good_taxons_nets
)

referentiel_nettoye_good <- liste_taxon_ajout %>%
  mutate(
    good_taxon_net = tolower(trimws(Cd_Taxon_norm))
  ) %>%
  select(good_taxon_net, Lb_Taxon) 

colnames(referentiel_nettoye_good)[colnames(referentiel_nettoye_good) == "Lb_Taxon"] <- "libelle_taxon"

good_taxons_avec_libelles <- dplyr::left_join(
  df_codes_pour_jointure_good,
  referentiel_nettoye_good,
  by = "good_taxon_net"
)

liste_libelles_good <- unique(good_taxons_avec_libelles$libelle_taxon)

if (length(liste_libelles_good) > 0) {
  liste_libelles_sans_na_good <- na.omit(liste_libelles_good)
  if (length(liste_libelles_sans_na_good) > 0) {
    print(liste_libelles_sans_na_good)
  } else {
    print("Aucun libellé correspondant trouvé pour les taxons sélectionnés.")
  }
} else {
  print("Aucun taxon ne dépasse le seuil pour la pression sur cet axe.")
}

df_to_export_bqualite <- data.frame(LibelleTaxon = liste_libelles_sans_na_good)
write.xlsx(df_to_export_bqualite, file = "Taxons_associes_bonne_qualite.xlsx", rowNames = FALSE)


# Traits
traits_bonne_qualite <- traits_weights_rlq[traits_weights_rlq[,1] > seuil_haut_trait, ]
if (nrow(traits_bonne_qualite) > 0) {
  traits_bonne_qualite_sorted <- traits_bonne_qualite[order(traits_bonne_qualite[,1], decreasing = FALSE),]
  print(traits_bonne_qualite_sorted)
} else {
  print("Aucun trait ne dépasse le seuil pour la bonne qualité sur cet axe.")
}

df_to_export_traits_bqualite <- data.frame(Trait = rownames(traits_bonne_qualite_sorted))
write.xlsx(df_to_export_traits_bqualite, file = "Traits_associes_bonne_qualite.xlsx", rowNames = FALSE)

########################################################################################
#                               NUTRIMENTS
#######################################################################################

nutrients_names <- c("1339", "1340", "1433","1335", "1350") # Exemple: Remplacez par vos noms exacts!


# Extraire les poids des nutriments
nutrients_weights <- env_parameters_weights_rlq[nutrients_names, ]
print(nutrients_weights)

# Calculer la "direction moyenne" des nutriments sur chaque axe.
mean_nutrient_direction_Ax1 <- mean(nutrients_weights[,1])
mean_nutrient_direction_Ax2 <- mean(nutrients_weights[,2])


#Automatisation du choix de l'axe le plus pertinent pour les nutriments
if (abs(mean_nutrient_direction_Ax1) >= abs(mean_nutrient_direction_Ax2)) {
  chosen_axis <- 1
  mean_nutrient_direction <- mean_nutrient_direction_Ax1
  cat("L'Axe 1 est choisi comme axe principal de la dégradation par les nutriments.\n")
} else {
  chosen_axis <- 2
  mean_nutrient_direction <- mean_nutrient_direction_Ax2
  cat("L'Axe 2 est choisi comme axe principal de la dégradation par les nutriments.\n")
}


seuil_bas_station <- quantile(stations_scores_rlq[, 1], probs = 0.25, na.rm = TRUE)
seuil_haut_station <- quantile(stations_scores_rlq[,1], probs = 0.75, na.rm = TRUE)


### Identification des stations, taxons et traits associés aux nutriments
  
#Identifier les stations dégradées par les nutriments

degraded_stations <- stations_scores_rlq[
  stations_scores_rlq[,chosen_axis] < 0 &                 
    stations_scores_rlq[,chosen_axis] < seuil_bas_station, ]  

# Tri par score croissant (les plus négatifs sont ceux qui sont les plus "bas" sur l'axe négatif)
degraded_stations_sorted <- degraded_stations[order(degraded_stations[,chosen_axis], decreasing = FALSE), ]
message_no_station <- paste0("Aucune station n'a de score entre 0 et -", seuil_bas_station, " sur l'Axe ", chosen_axis, ".")


if (nrow(degraded_stations_sorted) > 0) {
  print(degraded_stations_sorted)
} else {
  print(message_no_station)
}

# Identifier les taxons associés aux nutriments
taxons_degraded_conditions <- taxons_scores_rlq[
  taxons_scores_rlq[,chosen_axis] < 0 &
    taxons_scores_rlq[,chosen_axis] < seuil_bas_taxon, ]
taxons_degraded_conditions_sorted <- taxons_degraded_conditions[order(taxons_degraded_conditions[,chosen_axis], decreasing = FALSE), ]
message_no_taxon <- paste0("Aucun taxon n'a de score entre 0 et -", seuil_bas_taxon, " sur l'Axe ", chosen_axis, ".")

if (nrow(taxons_degraded_conditions_sorted) > 0) {
  print(taxons_degraded_conditions_sorted)
} else {
  print(message_no_taxon)
}

bad_taxons_affiches <- rownames(taxons_degraded_conditions_sorted)

bad_taxons_nets <- tolower(trimws(bad_taxons_affiches))

df_codes_pour_jointure_bad <- data.frame(
  bad_taxon_net = bad_taxons_nets
)

referentiel_nettoye_bad <- liste_taxon_ajout %>%
  mutate(
    bad_taxon_net = tolower(trimws(Cd_Taxon_norm))
  ) %>%
  select(bad_taxon_net, Lb_Taxon) 

colnames(referentiel_nettoye_bad)[colnames(referentiel_nettoye_bad) == "Lb_Taxon"] <- "libelle_taxon"

bad_taxons_avec_libelles <- dplyr::left_join(
  df_codes_pour_jointure_bad,
  referentiel_nettoye_bad,
  by = "bad_taxon_net"
)

liste_libelles_bad <- unique(bad_taxons_avec_libelles$libelle_taxon)

if (length(liste_libelles_bad) > 0) {
  liste_libelles_sans_na_bad <- na.omit(liste_libelles_bad)
  if (length(liste_libelles_sans_na_bad) > 0) {
    print(liste_libelles_sans_na_bad)
  } else {
    print("Aucun libellé correspondant trouvé pour les taxons sélectionnés.")
  }
} else {
  print("Aucun taxon ne dépasse le seuil pour la pression sur cet axe.")
}

df_to_export_bad_taxons <- data.frame(LibelleTaxon = liste_libelles_sans_na_bad)
write.xlsx(df_to_export_bad_taxons, file = "Taxons_associes_mauvaise_qualite_nutriments.xlsx", rowNames = FALSE)


# Identifier les traits associés aux nutriments

traits_degraded_conditions <- traits_weights_rlq[
  traits_weights_rlq[,chosen_axis] < 0 &
    traits_weights_rlq[,chosen_axis] < seuil_bas_trait, ]
traits_degraded_conditions_sorted <- traits_degraded_conditions[order(traits_degraded_conditions[,chosen_axis], decreasing = FALSE), ]
message_no_trait <- paste0("Aucun trait n'a de poids entre 0 et -", seuil_bas_trait, " sur l'Axe ", chosen_axis, ".")

if (nrow(traits_degraded_conditions_sorted) > 0) {
  print(traits_degraded_conditions_sorted)
} else {
  print(message_no_trait)
}

df_to_export_bad_traits <- data.frame(Trait = rownames(traits_degraded_conditions_sorted))
write.xlsx(df_to_export_bad_traits, file = "Traits_associes_mauvaise_qualite_nutriments.xlsx", rowNames = FALSE)

# Visualisation spécifique du gradient de nutriments

max_abs_score_station <- max(abs(stations_scores_rlq[,1]), abs(stations_scores_rlq[,2]))
max_abs_score_taxon <- max(abs(taxons_scores_rlq[,1]), abs(taxons_scores_rlq[,2]))
max_abs_weight_combined <- max(abs(env_parameters_weights_rlq), abs(traits_weights_rlq))

# Scaling basé sur l'étendue des stations et taxons

scaling_factor_viz <- max(max_abs_score_station, max_abs_score_taxon) / max_abs_weight_combined * 0.8

par(mar = c(5, 5, 4, 2) + 0.1)
plot(stations_scores_rlq, type = "n", asp = 1,
     xlim = range(stations_scores_rlq[,1], taxons_scores_rlq[,1], env_parameters_weights_rlq[,1] * scaling_factor_viz, traits_weights_rlq[,1] * scaling_factor_viz)*1.1,
     ylim = range(stations_scores_rlq[,2], taxons_scores_rlq[,2], env_parameters_weights_rlq[,2] * scaling_factor_viz, traits_weights_rlq[,2] * scaling_factor_viz)*1.1,
     xlab = paste0("Axe RLQ 1 (", round(rlq_result$eig[1]/sum(rlq_result$eig)*100, 2), "%)"),
     ylab = paste0("Axe RLQ 2 (", round(rlq_result$eig[2]/sum(rlq_result$eig)*100, 2), "%)"),
     main = "RLQ: Stations, Taxons, Traits & Nutriments")

# Ajouter les stations
points(stations_scores_rlq, pch = 20, col = "blue", cex = 0.8)


# Ajouter les taxons
points(taxons_scores_rlq, pch = 3, col = "darkgreen", cex = 0.8)

# Ajouter les paramètres environnementaux, en surlignant les nutriments
for (i in 1:nrow(env_parameters_weights_rlq)) {
  col_arrow <- "darkgrey" # Couleur par défaut
  lwd_arrow <- 1
  cex_label <- 0.7
  if (rownames(env_parameters_weights_rlq)[i] %in% nutrients_names) {
    col_arrow <- "darkred" # Couleur spécifique pour les nutriments
    lwd_arrow <- 2
    cex_label <- 0.9 # Label plus grand pour les nutriments
  }
  arrows(0, 0, env_parameters_weights_rlq[i,1] * scaling_factor_viz, env_parameters_weights_rlq[i,2] * scaling_factor_viz,
         length = 0.1, angle = 20, col = col_arrow, lwd = lwd_arrow)
  text(env_parameters_weights_rlq[i,1] * scaling_factor_viz * 1.1, env_parameters_weights_rlq[i,2] * scaling_factor_viz * 1.1,
       labels = rownames(env_parameters_weights_rlq)[i], cex = cex_label, col = col_arrow)
}

# Ajouter les traits
arrows(0, 0, traits_weights_rlq[,1] * scaling_factor_viz, traits_weights_rlq[,2] * scaling_factor_viz,
       length = 0.1, angle = 20, col = "purple", lwd = 1.5)
text(traits_weights_rlq[,1] * scaling_factor_viz * 1.1, traits_weights_rlq[,2] * scaling_factor_viz * 1.1,
     labels = rownames(traits_weights_rlq), cex = 0.7, col = "purple")

# Ajouter une légende
legend("topleft",
       legend = c("Stations", "Taxons", "Nutriments", "Autres param.env.", "Traits Écologiques"),
       pch = c(20, 3, NA, NA, NA),
       lty = c(NA, NA, 1, 1, 1),
       lwd = c(NA, NA, 2, 1, 1.5),
       col = c("blue", "darkgreen", "darkred", "darkgrey", "purple"),
       bty = "n", cex = 0.8)


