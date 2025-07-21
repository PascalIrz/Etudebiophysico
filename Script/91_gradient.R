load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/80_donnees_globales_trans.rda")
load(file = "Data/90_acp.rda")
load(file ="Data/40_statistiques_descriptives.rda")

library(sf)
library(ggplot2)
library(dplyr)
library(mapview)
library(ggspatial)
library(COGiter)
library(webshot)
webshot::install_phantomjs()


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

departement_breton <- departements_metro_geo %>%
  filter(DEP %in% c("22","29","35","56")) %>%
  st_transform(crs = 4326)

map_dim1 <- map_dim(gradient_sf, "Dim.1")
map_dim1

mapshot(map_dim1, file="gradient_de_pression.png",
        width = 1200, vheight = 1000,
        cliprect = "viewport")


map_dim2 <- map_dim(gradient_sf, "Dim.2")
map_dim2
map_dim3 <- map_dim(gradient_sf, "Dim.3")
map_dim3
map_dim4 <- map_dim(gradient_sf, "Dim.4")
map_dim4
map_dim5 <- map_dim(gradient_sf, "Dim.5")
map_dim5


#############################################################################
#                                  Nitrites, nitrates et ammonium
###############################################################################

#on calcule les percentiles 90 

p90 <- parametres_physico %>% 
  group_by(code_station_hydrobio,code_parametre) %>% 
  summarise(P90 = quantile(resultat, probs = 0.9, na.rm = TRUE))

p10 <- parametres_physico %>% 
  group_by(code_station_hydrobio,code_parametre) %>% 
  summarise(P10 = quantile(resultat, probs = 0.1, na.rm = TRUE))
  

# Nitrites 

classenit<-c(
  "Très bon" = "darkgreen",
  "Bon" = "lightgreen",
  "Moyen" = "yellow",
  "Médiocre"="orange",
  "Mauvais"="red"
)

nit_carte <- p90 %>%
  filter(code_parametre == "1339") %>%
  arrange(code_station_hydrobio) %>% 
  mutate(classe_nitrites= case_when(... = 
                                P90 <= 0.1 ~ "Très bon",
                                P90 <= 0.3 ~ "Bon",
                                P90 <= 0.5 ~ "Moyen",
                                P90 <= 1 ~ "Médiocre",
                                P90 > 1   ~ "Mauvais"
                                
                                
))


nit_carte <- nit_carte %>%
  mutate(classe_nitrites=factor(classe_nitrites,levels=c("Très bon","Bon","Moyen","Médiocre","Mauvais")))

gradient_nit <- nit_carte %>% 
  left_join(clean_ibd %>% 
            dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
            distinct(),
            by = "code_station_hydrobio"
) 

mapview(gradient_nit, zcol="classe_nitrites", col.regions=classenit, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

gradient_nit_sf <- st_as_sf(gradient_nit, coords=c("longitude","latitude"),crs=4326)

ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = gradient_nit_sf, aes(color = classe_nitrites), size = 2) +
  scale_color_manual(values = classenit) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  coord_sf(xlim = c(-5.2, -1), ylim = c(47, 49), expand = FALSE) +
  labs(title = "Etat chimique selon les concentrations en nitrites (P90) dans les cours d'eau bretons, 2015-2023.",
       color = "Classe d'état") +
  theme_minimal() +
  theme(legend.position = "bottom")




#Nitrates 

classe_nitt <- c(
  "Très bon" = "darkgreen",
  "Bon" = "lightgreen",
  "Hors classement"="red"
)

nitrates_carte <- p90 %>%
  filter(code_parametre == "1340") %>%
  arrange(code_station_hydrobio) %>% 
  mutate(classe_nitrates= case_when(... = 
                                      P90 <= 10 ~ "Très bon",
                                    P90 <= 50 ~ "Bon",
                                    P90 > 50 ~ "Hors classement"
                                   
                                    
  ))


nitrates_carte <- nitrates_carte %>%
  mutate(classe_nitrates=factor(classe_nitrates,levels=c("Très bon","Bon","Hors classement")))

gradient_nitrates <- nitrates_carte %>% 
  left_join(clean_ibd %>% 
              dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
              distinct(),
            by = "code_station_hydrobio"
  ) 

mapview(gradient_nitrates, zcol="classe_nitrates", col.regions=classe_nitt, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

nitrates_sf<-st_as_sf(gradient_nitrates, coords=c("longitude","latitude"),crs=4326)

ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = nitrates_sf, aes(color = classe_nitrates), size = 2) +
  scale_color_manual(values = classe_nitt) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  coord_sf(xlim = c(-5.2, -1), ylim = c(47, 49), expand = FALSE) +
  labs(title = "Gradient des concentrations en nitrates sur le territoire breton",
       color = "Classe d'état") +
  theme_minimal() +
  theme(legend.position = "bottom")


#Ammonium 

classe_nh4 <- c(
  "Très bon" = "darkgreen",
  "Bon" = "lightgreen",
  "Moyen" = "yellow",
  "Médiocre" = "orange",
  "Mauvais"="red"
)

ammonium_carte <- p90 %>%
  filter(code_parametre == "1335") %>%
  arrange(code_station_hydrobio) %>% 
  mutate(classe_ammonium= case_when(... = 
                                      P90 <= 0.1 ~ "Très bon",
                                    P90 <= 0.5 ~ "Bon",
                                    P90 <= 2 ~ "Moyen",
                                    P90 <= 5 ~ "Médiocre",
                                    P90 > 5 ~ "Mauvais"
                                    
                                    
  ))


ammonium_carte <- ammonium_carte %>%
  mutate(classe_ammonium=factor(classe_ammonium,levels=c("Très bon","Bon","Moyen","Médiocre","Mauvais")))

gradient_ammonium <- ammonium_carte %>% 
  left_join(clean_ibd %>% 
              dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
              distinct(),
            by = "code_station_hydrobio"
  ) 

mapview(gradient_ammonium, zcol="classe_ammonium", col.regions=classe_nh4, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

ammonium_sf<-st_as_sf(gradient_ammonium, coords=c("longitude","latitude"),crs=4326)

ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ammonium_sf, aes(color = classe_ammonium), size = 2) +
  scale_color_manual(values = classe_nh4) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  coord_sf(xlim = c(-5.2, -1), ylim = c(47, 49), expand = FALSE) +
  labs(title = "Etat chimique selon les concentrations en ammonium (P90) dans les cours d'eau bretons, 2015-2023.",
       color = "Classe d'état") +
  theme_minimal() +
  theme(legend.position = "bottom")


#############################################################################
#                                  Ptot et PO4-
###############################################################################

#PTOT 

classe_ptotal <- c(
  "Très bon" = "darkgreen",
  "Bon" = "lightgreen",
  "Moyen" = "yellow",
  "Médiocre" = "orange",
  "Mauvais"="red"
)

ptot_carte <- p90 %>%
  filter(code_parametre == "1350") %>%
  arrange(code_station_hydrobio) %>% 
  mutate(classe_ptot= case_when(... = 
                                      P90 <= 0.05 ~ "Très bon",
                                    P90 <= 0.2 ~ "Bon",
                                    P90 <= 0.5 ~ "Moyen",
                                    P90 <= 1 ~ "Médiocre",
                                    P90 > 1 ~ "Mauvais"
                                    
                                    
  ))


ptot_carte <- ptot_carte %>%
  mutate(classe_ptot =factor(classe_ptot,levels=c("Très bon","Bon","Moyen","Médiocre","Mauvais")))

gradient_ptot <- ptot_carte %>% 
  left_join(clean_ibd %>% 
              dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
              distinct(),
            by = "code_station_hydrobio"
  ) 

mapview(gradient_ptot, zcol="classe_ptot", col.regions=classe_ptotal, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

ptot_sf<-st_as_sf(gradient_ptot, coords=c("longitude","latitude"),crs=4326)

ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ptot_sf, aes(color = classe_ptot), size = 2) +
  scale_color_manual(values = classe_ptotal) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  coord_sf(xlim = c(-5.2, -1), ylim = c(47, 49), expand = FALSE) +
  labs(title = "Etat chimique selon les concentrations en phosphore total (P90) dans les cours d'eau bretons, 2015-2023.",
       color = "Classe d'état") +
  theme_minimal() +
  theme(legend.position = "bottom")

#PO4
classe_po4 <- c(
  "Très bon" = "darkgreen",
  "Bon" = "lightgreen",
  "Moyen" = "yellow",
  "Médiocre" = "orange",
  "Mauvais"="red"
)

po4_carte <- p90 %>%
  filter(code_parametre == "1433") %>%
  arrange(code_station_hydrobio) %>% 
  mutate(classe_po4color = case_when(... = 
                                  P90 <= 0.1 ~ "Très bon",
                                P90 <= 0.5 ~ "Bon",
                                P90 <= 1 ~ "Moyen",
                                P90 <= 2 ~ "Médiocre",
                                P90 > 2 ~ "Mauvais"
                                
                                
  ))


po4_carte <- po4_carte %>%
  mutate(classe_po4color =factor(classe_po4color,levels=c("Très bon","Bon","Moyen","Médiocre","Mauvais")))

gradient_po4 <- po4_carte %>% 
  left_join(clean_ibd %>% 
              dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
              distinct(),
            by = "code_station_hydrobio"
  ) 

mapview(gradient_po4, zcol="classe_po4color", col.regions=classe_po4, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

po4_sf<-st_as_sf(gradient_po4, coords=c("longitude","latitude"),crs=4326)

ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = po4_sf, aes(color = classe_po4color), size = 2) +
  scale_color_manual(values = classe_ptotal) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  coord_sf(xlim = c(-5.2, -1), ylim = c(47, 49), expand = FALSE) +
  labs(title = "Gradient des concentrations en orthophosphates sur le territoire breton",
       color = "Classe d'état") +
  theme_minimal() +
  theme(legend.position = "bottom")

#############################################################################
#                                  Taux de saturations en oxygène  
###############################################################################

classe_oxy <- c(
  "Très bon" = "darkgreen",
  "Bon" = "lightgreen",
  "Moyen" = "yellow",
  "Médiocre"="orange",
  "Mauvais"="red"
)

oxy_carte <- p10 %>%
  filter(code_parametre == "1311") %>%
  arrange(code_station_hydrobio) %>% 
  mutate(classe_o2= case_when(... = 
                                      P10 >= 8 ~ "Très bon",
                                    P10 >= 6 ~ "Bon",
                                    P10 >= 4 ~ "Moyen",
                                    P10 >= 3 ~ "Médiocre",
                                    P10 < 3 ~ "Mauvais"
                                    
                                    
  ))


oxy_carte <- oxy_carte %>%
  mutate(classe_o2=factor(classe_o2,levels=c("Très bon","Bon","Moyen","Médiocre","Mauvais")))

gradient_oxy <- oxy_carte %>% 
  left_join(clean_ibd %>% 
              dplyr::select(code_station_hydrobio, longitude, latitude) %>% 
              distinct(),
            by = "code_station_hydrobio"
  ) 

mapview(gradient_oxy, zcol="classe_o2", col.regions=classe_oxy, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

oxy_sf<-st_as_sf(gradient_oxy, coords=c("longitude","latitude"),crs=4326)

ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = oxy_sf, aes(color = classe_o2), size = 2) +
  scale_color_manual(values = classe_oxy) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  coord_sf(xlim = c(-5.2, -1), ylim = c(47, 49), expand = FALSE) +
  labs(title = "Etat chimique selon les concentrations en oxygène dissous (P90) dans les cours d'eau bretons, 2015-2023.",
       color = "Classe d'état") +
  theme_minimal() +
  theme(legend.position = "bottom")
