# Chargement des df et librairies 

load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/50_chroniques_et_tendance.rda")
library(sf)
library(tidyverse)
library(mapview)
library(ggplot2)
library(ggspatial)
library(COGiter)
#################################################################################
#                       I2M2                               #
#################################################################################

couleursi2m2<-c(
  "Très bon" = "darkgreen",
  "Bon" = "lightgreen",
  "Moyen" = "yellow",
  "Médiocre"="orange",
  "Mauvais" = "red"
  
)


i2m2test <- i2m2 %>%
  mutate(classe_etat= case_when(... = 
                                  resultat_indice > 0.665 ~ "Très bon",
                                resultat_indice > 0.443 ~ "Bon",
                                resultat_indice > 0.295 ~ "Moyen",
                                resultat_indice > 0.148 ~ "Médiocre",
                                TRUE                    ~ "Mauvais"
                                
  ))

i2m2test<- i2m2test %>%
  mutate(classe_etat=factor(classe_etat,levels=c("Très bon","Bon","Moyen","Médiocre","Mauvais")))


#On teste avec facet_wrap

i2m2_sf<-st_as_sf(i2m2test, coords=c("longitude","latitude"),crs=4326)

departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)


ggplot() + 
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = i2m2_sf, aes(color = classe_etat), size = 2) +
  scale_color_manual(values = couleursi2m2) +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  coord_sf(xlim = c(-5.2, -1), ylim = c(47, 49), expand = FALSE) +
  facet_wrap(~ as.factor(annee)) +
  labs(title = "La qualité biologique des cours d'eau bretons selon l'I2M2, 2015-2023",
       color = "Classe d'état") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold"), 
    legend.title = element_text(size = 14, face = "bold"), 
    legend.text = element_text(size = 12) 
  )

#################################################################################
#                       IBD                               #
#################################################################################

#On crée les couleurs par classe
couleursibd<-c(
  "Très bon" = "darkgreen",
  "Bon" = "lightgreen",
  "Moyen" = "yellow",
  "Médiocre"="orange",
  "Mauvais" = "red"
  
)

#On va regarder l'évolution de la classe d'i2m2 au cours du temps 
ibdtest <- ibd %>%
  mutate(classe_etat= case_when(... = 
                                  resultat_indice > 16.4 ~ "Très bon",
                                resultat_indice > 13.8 ~ "Bon",
                                resultat_indice > 10 ~ "Moyen",
                                resultat_indice > 5.9 ~ "Médiocre",
                                TRUE                    ~ "Mauvais"
                                
  ))

# On classe les ordres dans l'ordre
ibdtest<- ibdtest %>%
  mutate(classe_etat=factor(classe_etat,levels=c("Très bon","Bon","Moyen","Médiocre","Mauvais")))

# Carte intéractive
mapview(ibdtest, zcol="classe_etat", col.regions=couleursi2m2, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

# On créé l'objet sf 
ibd_sf<-st_as_sf(ibdtest, coords=c("longitude","latitude"),crs=4326)

# Départements bretons à partir de COGITER
departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)

# Cartes 
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ibd_sf, aes(color = classe_etat), size = 2) +
  scale_color_manual(values = couleursibd) +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  coord_sf(xlim = c(-5.2, -1), ylim = c(47, 49), expand = FALSE) +
  facet_wrap(~ as.factor(annee)) +
  labs(title = "La qualité biologique des cours d'eau bretons selon l'IBD, 2008-2023",
       color = "Classe d'état") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold"), 
    legend.title = element_text(size = 14, face = "bold"), 
    legend.text = element_text(size = 12) 
  )

# Calcul du pourcentage de stations classées comme médiocre en 2023
# On filtre l'année 2023
ibdtest_2023 <- i2m2test %>% 
  filter(annee == "2023") %>% 
  distinct(code_station_hydrobio, classe_etat)

# Nombre total de résultats en 2023
nb_total <- nrow(ibdtest_2023)

#Nombre de stations classées comme médiocre
nb_mediocre <- ibdtest_2023 %>% 
  filter(classe_etat == "Moyen") %>% 
  nrow()

# Pourcentage de stations médiocre en 2023
proportion_mediocre <- nb_mediocre / nb_total

save(i2m2test, file="Data/classe_i2m2.rda")
