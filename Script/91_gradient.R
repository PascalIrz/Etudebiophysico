load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/81_donnees_globales_trans.rda")
load(file = "Data/81_acp_rda")
load(file ="Data/40_statistiques_descriptives.rda")

library(sf)
library(dplyr)
library(mapview)

functions <- list.files(path = "R",
                        pattern = ".R$",
                        full.names = TRUE)

map(.x = functions,
    .f = source)

#############################################################################
#                                  Physico inter-stations 
###############################################################################

acp_liste_pc <- physico_wide %>% 
  mutate(
    num_line = row_number()) %>% 
  arrange(code_station_hydrobio) %>% 
  dplyr::select(code_station_hydrobio,num_line)
  

dim_acp <- as.data.frame(resultat_acp_physico_2[["ind"]][["coord"]]) %>% 
  mutate(
    num_line = row_number()) 

gradient <- acp_liste_pc %>% 
  left_join(dim_acp, by = "num_line") %>% 
  left_join(clean_ibd %>% 
              dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
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


#############################################################################
#                                  Nitrites et nitrates  
###############################################################################

# Nitrites 

classenit<-c(
  "1" = "darkgreen",
  "2" = "yellow",
  "3" = "orange",
  "4"="red"
)

nit_carte <- med_par_station_pc %>%
  filter(code_parametre == "1339") %>%
  arrange(code_station_hydrobio) %>% 
  mutate(classe_nitrites= case_when(... = 
                                med <= 0.01 ~ "1",
                                med >= 0.01 ~ "2",
                                med >= 0.1 ~ "3",
                                med >= 1 ~ "4"
                                
))


nit_carte <- nit_carte %>%
  mutate(classe_nitrites=factor(classe_nitrites,levels=c("1","2","3","4")))

gradient_nit <- nit_carte %>% 
  left_join(clean_ibd %>% 
            dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
            distinct(),
            by = "code_station_hydrobio"
) 

mapview(gradient_nit, zcol="classe_nitrites", col.regions=classenit, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)


#Nitrates 

classe_nitt <- c(
  "1" = "darkgreen",
  "2" = "lightgreen",
  "3" = "yellow",
  "4"="orange",
  "5"="darkorange3",
  "6"="red"
)

nitrates_carte <- med_par_station_pc %>%
  filter(code_parametre == "1340") %>%
  arrange(code_station_hydrobio) %>% 
  mutate(classe_nitrates= case_when(... = 
                                      med < 0.1 ~ "1",
                                    med < 3 ~ "2",
                                    med < 5 ~ "3",
                                    med < 10 ~ "4",
                                    med < 15 ~ "5",
                                    med >= 15 ~ "6"
                                    
  ))


nitrates_carte <- nitrates_carte %>%
  mutate(classe_nitrates=factor(classe_nitrates,levels=c("1","2","3","4","5","6")))

gradient_nitrates <- nitrates_carte %>% 
  left_join(clean_ibd %>% 
              dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
              distinct(),
            by = "code_station_hydrobio"
  ) 

mapview(gradient_nitrates, zcol="classe_nitrates", col.regions=classe_nitt, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)


#############################################################################
#                                  Taux de saturations en oxygène  
###############################################################################

classe_oxy <- c(
  "1" = "darkgreen",
  "2" = "lightgreen",
  "3" = "yellow",
  "4"="orange",
  "5"="darkorange3",
  "6"="red"
)

tsat_carte <- med_par_station_pc %>%
  filter(code_parametre == "1312") %>%
  arrange(code_station_hydrobio) %>% 
  mutate(classe_tsat= case_when(... = 
                                      med >= 90 ~ "1",
                                    med < 90 ~ "2",
                                    med < 70 ~ "3",
                                    med < 50 ~ "4",
                                    med < 30 ~ "5",
                                    med < 10 ~ "6"
                                    
  ))


tsat_carte <- tsat_carte %>%
  mutate(classe_tsat=factor(classe_tsat,levels=c("1","2","3","4","5","6")))

gradient_tsat <- tsat_carte %>% 
  left_join(clean_ibd %>% 
              dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
              distinct(),
            by = "code_station_hydrobio"
  ) 

mapview(gradient_tsat, zcol="classe_tsat", col.regions=classe_oxy, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

