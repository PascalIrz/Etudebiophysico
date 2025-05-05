load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/81_donnees_globales_trans.rda")
load(file = "Data/81_acp_rda")

library(sf)
library(dplyr)
library(mapview)

#############################################################################
#                                  Physico inter-stations 
###############################################################################

acp_liste_pc <- physico_wide %>% 
  mutate(
    num_line = row_number()) %>% 
  arrange(code_station_hydrobio) %>% 
  select(code_station_hydrobio,num_line)
  

dim_acp <- as.data.frame(resultat_acp_physico_2[["ind"]][["coord"]]) %>% 
  mutate(
    num_line = row_number()) 

gradient <- acp_liste_pc %>% 
  left_join(dim_acp, by = "num_line") %>% 
  left_join(clean_ibd %>% 
              select(code_station_hydrobio, longitude, latitude) %>% 
              distinct(),
            by = "code_station_hydrobio"
            )

gradient_sf <- gradient %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

map_dim1 <- map_dim(gradient_sf, "Dim.1")
map_dim1
map_dim2 <- map_dim(gradient_sf, "Dim.2")
map_dim2
map_dim3 <- map_dim(gradient_sf, "Dim.3")
map_dim3
map_dim4 <- map_dim(gradient_sf, "Dim.4")
map_dim4
map_dim5 <- map_dim(gradient_sf, "Dim.5")
map_dim5





  

