# Chargement des librairies et données
load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/70_choix_parametre.rda")
library(tidyverse)

#On calcule la moyenne par indice et station I2M2
i2m2_moy <- clean_minv %>%
  group_by(code_indice, code_station_hydrobio) %>%
  summarise(moy_i2m2 = mean(resultat_indice, na.rm = TRUE),
            .groups = "drop")

# On calcule la moyenne par indice et station IBD et IPS 
ibd_moy <- clean_ibd %>% 
  group_by(code_indice, code_station_hydrobio) %>%
  summarise(moy_ibd = mean(resultat_indice, na.rm = TRUE),
            .groups = "drop")

# On fait des correspondances entre les paramètres physico et les périodes retenues
periode_param <- tibble::tibble(
  code_parametre = c("1295", "1302", "1303", "1305","1339","1313", "1350","1841", "1335", "1433","1301", "1312","1311", "1340"),
  periode = c("annuelle", "annuelle", "annuelle", "annuelle","annuelle", "annuelle","annuelle", "annuelle", "annuelle","annuelle","annuelle", "mai_decembre", "mai_decembre","janvier_mars")
)

# On crée le jeu de données à partir des moyennes calculées dans le script précedent
mean_physico_periode <- mean_physico %>%
  left_join(periode_param, by = "code_parametre") %>%
  filter(
    (periode == "annuelle") |
      (periode == "mai_decembre" & mois >= 5 & mois <= 12) |
      (periode == "janvier_mars" & mois >= 1 & mois <= 3)
  )

#On calcule les moyennes de chaque paramètres physico-chimiques sur les périodes retenues
physico_moyenne_annee <- mean_physico_periode %>% 
  group_by(code_station_hydrobio,annee,code_parametre) %>%
  summarise(para_moy_periode = mean(para_moy, na.rm = TRUE), .groups = "drop")

# On sauvegarde
save(physico_moyenne_annee, 
     file = "Data/physico_moyenne_annuelle.rda"
       )

# Calcul des moyennes interannuelles de chaque paramètre physico-chimique
resultat_physico_final <- mean_physico_periode %>%
  group_by(code_station_hydrobio, code_parametre) %>%
  summarise(para_moy_periode = mean(para_moy, na.rm = TRUE), .groups = "drop")

# On met l'indice et métriques en colonne 
i2m2_wide <- i2m2_moy %>% 
  pivot_wider(names_from = code_indice, 
              values_from = moy_i2m2)

# On met les paramètres physico-chimiques en colonne
physico_wide <- resultat_physico_final %>%
  pivot_wider(names_from = code_parametre,
              values_from = para_moy_periode,
              values_fn = mean)

# On met les indices en colonnes
ibd_wide <- ibd_moy %>% 
  arrange(code_station_hydrobio) %>% 
  pivot_wider(names_from = code_indice,
              values_from = moy_ibd)  

# On joint tous les jeux de données ensemble, pour la modélisation
df_global <- i2m2_wide %>% 
  left_join(ibd_wide, by = "code_station_hydrobio") %>% 
  left_join(physico_wide, by= "code_station_hydrobio")


save(
  df_global,
  ibd_wide,
  i2m2_wide,
  physico_wide,
  file = "Data/80_donnees_globales_trans.rda"
)




