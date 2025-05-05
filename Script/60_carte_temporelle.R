load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/50_chroniques_et_tendance.rda")
library(sf)
library(dplyr)
library(mapview)
library(ggplot2)
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

ggplot(ibd_sf) + 
  geom_sf(aes(color = classe_etat), size = 3) +
  scale_color_manual(values = couleursi2m2) + 
  facet_wrap(~ as.factor(annee)) +
  labs(title = "Évolution des classes d'état par année",
       color = "Classe d'état") +
  theme_minimal()



#################################################################################
#                       Physico                              #
#################################################################################

#seuils ??? avec moyenne choisie
