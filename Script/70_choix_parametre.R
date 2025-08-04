# Chargement librairies, fonctions et données 
library(tidyverse)
library(purrr)
library(ggplot2)
library(corrplot)
source (file = "R/calculer_indicateur.R")

#Constitution des combinaisons de mois : avoir toutes les combinaisons possibles
#entre le mois de début et le mois de fin
combin_mois <- expand.grid(1:12, 1:12) %>%
  set_names("mois_debut", "mois_fin") %>%
  filter(mois_debut <= mois_fin)

#Vérification
combin_mois %>%
  mutate(remp = 1) %>%
  pivot_wider(names_from = mois_fin,
              values_from = remp)

#Assemblage du jeu de données 

window_data <- expand.grid(stations_parametre,
                           code_pc,
                           2015:2024,
                           1:12) %>%
  set_names("code_station_hydrobio", "code_parametre", "annee", "mois")

mean_physico <- parametres_physico %>% 
  group_by(code_station_hydrobio,
         code_parametre,
         annee,
         mois) %>% 
  summarise(para_moy = mean(resultat, na.rm = TRUE))


donnees_jointure <- window_data %>% 
  left_join(y = mean_physico)


glimpse(window_data)


# test pour toutes les combinaisons de mois de début (.x) et de fin (.y)

resultat2 <- map2_df(
  .df = donnees_jointure,
  .f = calculer_indicateur,
  .var_mois = mois,
  .var_valeur = para_moy,
  .x = combin_mois$mois_debut,
  .y = combin_mois$mois_fin,
  code_station_hydrobio,
  code_parametre
)

indices_moy_par_sta <- clean_minv %>% 
  group_by(code_station_hydrobio,
           code_indice) %>% 
  summarise(indice_moy = mean(resultat_indice, na.rm = TRUE),
            .groups = "drop")

resultat_i2m2 <- resultat2 %>%
  rename(para_moy = moy) %>% 
  left_join(y = indices_moy_par_sta)
            
cor_i2m2 <- resultat_i2m2 %>% 
  drop_na() %>% 
  group_by(code_parametre,
           code_indice,
           debut,
           fin) %>% 
  summarise(correlation = cor.test(para_moy,
                              indice_moy,
                              method = "spearman")$estimate,
            p_value = cor.test(para_moy,indice_moy, method="spearman")$p.value,
            .groups = "drop"
            )

cor_i2m2 %>% 
  ggplot(aes(x = debut,
             y = fin,
             fill = correlation,
             col = correlation)) +
  geom_point(size = 3) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  scale_color_distiller(palette = "Spectral", direction = 1) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(breaks = 1:12) +
  facet_grid(code_indice~code_parametre) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Mois de début",
       y = "Mois de fin")
  
  
#Ajouter les noms
cor_i2m2_label <- cor_i2m2 %>%
  mutate(
    nom_parametre = case_when(
      code_parametre == "1301" ~ "T°",
      code_parametre == "1302" ~ "pH",
      code_parametre == "1303" ~ "Conductiv",
      code_parametre == "1305" ~ "MES",
      code_parametre == "1311" ~ "O2 dissous",
      code_parametre == "1312" ~ "Satur.O2",
      code_parametre == "1313" ~ "DBO5",
      code_parametre == "1335" ~ "NH4+",
      code_parametre == "1339" ~ "NO2-",
      code_parametre == "1340" ~ "NO3-",
      code_parametre == "1350" ~ "P total",
      code_parametre == "1433" ~ "PO4-",
      code_parametre == "1295" ~ "Turbidité",
      code_parametre == "1841" ~ "C organique",
      TRUE ~ code_parametre
    ),
    nom_indice = case_when(
      code_indice == "7613" ~ "I2M2",
      code_indice == "8054" ~ "RichesI2M2",
      code_indice == "8055" ~ "OvovivI2M2",
      code_indice == "8056" ~ "PolyvolI2M2",
      code_indice == "8057" ~ "ASPT",
      code_indice == "8058" ~ "H'",
      TRUE ~ code_indice
    ),
    nom_parametre = factor(nom_parametre, levels= c("NH4+","NO2-","NO3-","P total","PO4-","C organique","O2 dissous","Satur.O2","DBO5","MES","Turbidité","Conductiv","pH","T°")),
    nom_indice = factor(nom_indice, levels= c("I2M2","H'","RichesI2M2","OvovivI2M2","PolyvolI2M2","ASPT"))
  )


#Création du graphique
cor_i2m2_label %>% 
ggplot(aes(x = debut, 
           y = fin, 
           fill = correlation, 
           col = correlation)) +
  geom_point(size = 3) +
  geom_point(data = cor_i2m2_label %>% filter(p_value < 0.01),
             aes(x = debut, y = fin),
             fill = NA, color = "grey", size = 0.5) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  scale_color_distiller(palette = "Spectral", direction = 1) +
  scale_x_continuous(breaks = seq(1, 12, by = 2)) +
  scale_y_continuous(breaks = seq(1, 12, by = 2)) +
    
  facet_grid(nom_indice ~ nom_parametre) +
    

  theme_bw() +
  theme(
    panel.grid.major = element_blank(),  # suppression du quadrillage principal
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
    

  labs(
    x = "Mois de début",
    y = "Mois de fin",
    fill = "Corrélation",
    color = "Corrélation"
  )

#################################################################################
#                       IBD                                                     #
#################################################################################

indices_moy_par_sta_ibd <- clean_ibd %>% 
  group_by(code_station_hydrobio,
           code_indice) %>% 
  summarise(indice_moy = mean(resultat_indice, na.rm = TRUE),
            .groups = "drop")

  
resultat_ibd <- resultat2 %>%
  rename(para_moy = moy) %>% 
  left_join(y = indices_moy_par_sta_ibd)


cor_ibd <- resultat_ibd %>% 
  drop_na() %>% 
  group_by(code_parametre,
           code_indice,
           debut,
           fin) %>% 
  summarise(correlation = cor.test(para_moy,
                                   indice_moy,
                                   method = "spearman")$estimate,
            p_value = cor.test(para_moy,indice_moy, method="spearman")$p.value,
            .groups = "drop"
  )

cor_ibd %>% 
  ggplot(aes(x = debut,
             y = fin,
             fill = correlation,
             col = correlation)) +
  geom_point(size = 3) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  scale_color_distiller(palette = "Spectral", direction = 1) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(breaks = 1:12) +
  facet_grid(code_indice~code_parametre) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Mois de début",
       y = "Mois de fin")

cor_ibd_label <- cor_ibd %>%
  mutate(
    nom_parametre = case_when(
      code_parametre == "1301" ~ "T°",
      code_parametre == "1302" ~ "pH",
      code_parametre == "1303" ~ "Conductiv",
      code_parametre == "1305" ~ "MES",
      code_parametre == "1311" ~ "O2 dissous",
      code_parametre == "1312" ~ "Satur.O2",
      code_parametre == "1313" ~ "DBO5",
      code_parametre == "1335" ~ "NH4+",
      code_parametre == "1339" ~ "NO2-",
      code_parametre == "1340" ~ "NO3-",
      code_parametre == "1350" ~ "P total",
      code_parametre == "1433" ~ "PO4-",
      code_parametre == "1295" ~ "Turbidité",
      code_parametre == "1841" ~ "C organique",
      TRUE ~ code_parametre
    ),
    nom_indice = case_when(
      code_indice == "1022" ~ "IPS",
      code_indice == "5856" ~ "IBD",
    ),
    nom_parametre = factor(nom_parametre, levels= c("NH4+","NO2-","NO3-","P total","PO4-","C organique","O2 dissous","Satur.O2","DBO5","MES","Turbidité","Conductiv","pH","T°")),
    nom_indice = factor(nom_indice, levels= c("IPS","IBD"))
  )

cor_ibd_label %>% 
  ggplot(aes(x = debut, 
             y = fin, 
             fill = correlation, 
             col = correlation)) +
  geom_point(size = 3) +
  geom_point(data = cor_ibd_label %>% filter(p_value < 0.05),
             aes(x = debut, y = fin),
             fill = NA, color = "grey", size = 0.5) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  scale_color_distiller(palette = "Spectral", direction = 1) +
  scale_x_continuous(breaks = seq(1, 12, by = 2)) +
  scale_y_continuous(breaks = seq(1, 12, by = 2)) +
  
  facet_grid(nom_indice ~ nom_parametre) +
  
  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),  # suppression du quadrillage principal
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  
  labs(
    x = "Mois de début",
    y = "Mois de fin",
    fill = "Corrélation",
    color = "Corrélation"
  )

save(mean_physico,
     file = "Data/70_choix_parametre.rda")


