load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/50_chroniques_et_tendance.rda")
library(sf)
library(dplyr)
library(mapview)
library(ggplot2)
library(ggspatial)
#################################################################################
#                       I2M2                               #
#################################################################################

#On crée les couleurs par classe
couleursi2m2<-c(
  "Très bon" = "darkgreen",
  "Bon" = "lightgreen",
   "Moyen" = "yellow",
  "Médiocre"="orange",
  "Mauvais" = "red"
  
)

#On va regarder l'évolution de la classe d'i2m2 au cours du temps 
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

mapview(i2m2test, zcol="classe_etat", col.regions=couleursi2m2, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

#On teste avec facet_wrap

i2m2_sf<-st_as_sf(i2m2test, coords=c("longitude","latitude"),crs=4326)

ggplot(i2m2_sf) + 
  geom_sf(aes(color = classe_etat), size = 3) +
  scale_color_manual(values = couleursi2m2) + 
  facet_wrap(~ as.factor(annee)) +
  labs(title = "Évolution des classes d'état par année",
       color = "Classe d'état") +
  theme_minimal()


#################################################################################
#                       IBD                               #
#################################################################################

remotes::install_github("MaelTheuliere/COGiter")
library(COGiter)


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

ibdtest<- ibdtest %>%
  mutate(classe_etat=factor(classe_etat,levels=c("Très bon","Bon","Moyen","Médiocre","Mauvais")))

mapview(ibdtest, zcol="classe_etat", col.regions=couleursi2m2, xcol="longitude", ycol="latitude", crs= 4326, grid=FALSE,cex=4.5)

#On teste avec facet_wrap

ibd_sf<-st_as_sf(ibdtest, coords=c("longitude","latitude"),crs=4326)


departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)


ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ibd_sf, aes(color = classe_etat), size = 2) +
  scale_color_manual(values = couleursibd) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  coord_sf(xlim = c(-5.2, -1), ylim = c(47.5, 49), expand = FALSE) +
  facet_wrap(~ as.factor(annee), ncol = 3) +
  
  labs(title = "Évolution des classes d'état IBD en Bretagne",
       color = "Classe d'état") +
  theme_minimal() +
  theme(legend.position = "bottom")



ggplot(ibd_sf) + 
  geom_sf(aes(color = classe_etat), size = 3) +
  scale_color_manual(values = couleursi2m2) + 
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  annotation_scale(location = "br", width_hint = 0.3) +
  facet_wrap(~ as.factor(annee)) +
  labs(title = "Évolution des classes d'état par année",
       color = "Classe d'état") +
  theme_minimal()

ibdtest_2023 <- i2m2test %>% 
  filter(annee == "2023") %>% 
  distinct(code_station_hydrobio, classe_etat)

nb_total <- nrow(ibdtest_2023)
nb_mediocre <- ibdtest_2023 %>% 
  filter(classe_etat == "Moyen") %>% 
  nrow()

proportion_mediocre <- nb_mediocre / nb_total
proportion_mediocre


save(i2m2test, file="Data/classe_i2m2.rda")
