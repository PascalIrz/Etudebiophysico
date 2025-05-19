load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/70_choix_parametre.rda")

library(dplyr)
library(tidyr)


i2m2_moy <- clean_minv %>%
  group_by(code_indice, code_station_hydrobio) %>%
  summarise(moy_i2m2 = mean(resultat_indice, na.rm = TRUE),
            .groups = "drop")

ibd_moy <- clean_ibd %>% 
  group_by(code_indice, code_station_hydrobio) %>%
  summarise(moy_ibd = mean(resultat_indice, na.rm = TRUE),
            .groups = "drop")



periode_param <- tibble::tibble(
  code_parametre = c("1295", "1302", "1303", "1305","1339","1313", "1350","1841", "1335", "1433","1301", "1312","1311", "1340"),
  periode = c("annuelle", "annuelle", "annuelle", "annuelle","annuelle", "annuelle","annuelle", "annuelle", "annuelle","annuelle","annuelle", "mars_decembre", "mars_decembre","janvier_mars")
)

mean_physico_periode <- mean_physico %>%
  left_join(periode_param, by = "code_parametre") %>%
  filter(
    (periode == "annuelle") |
      (periode == "mars_decembre" & mois >= 3 & mois <= 12) |
      (periode == "janvier_mars" & mois >= 1 & mois <= 3)
  )


resultat_physico_final <- mean_physico_periode %>%
  group_by(code_station_hydrobio, code_parametre) %>%
  summarise(para_moy_periode = mean(para_moy, na.rm = TRUE), .groups = "drop")

i2m2_wide <- i2m2_moy %>% 
  pivot_wider(names_from = code_indice, 
              values_from = moy_i2m2)

physico_wide <- resultat_physico_final %>%
  pivot_wider(names_from = code_parametre,
              values_from = para_moy_periode,
              values_fn = mean)

ibd_wide <- ibd_moy %>% 
  arrange(code_station_hydrobio) %>% 
  pivot_wider(names_from = code_indice,
              values_from = moy_ibd)  
  

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

# alternative : expand.grid pour avoir les 78 combinaisons possibles par paramètres (même structure que tableau transposé),
#juste les var qui me sert à créer les combinaisons de variables
#left_join(y=tableau de données)
#valeurs manquantes
#goup_by (indique les combinaisons où il ya  des valeurs manquantes et ifelse (pour les remplir)




