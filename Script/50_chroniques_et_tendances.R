#Chargement des données 
load(file = "Data/10_donnees_pretraitees.rda")
source(file = "R/mk_st_by_group.R")
source(file = "R/Mann_kendall_div.R")

#Librairies
library(tidyverse)
library(trend)
library(sf)
library(mapview)
library(scales)
library(ggplot2)
library(COGiter)
library(ggspatial)

# On charge les fonctions utiles
functions <- list.files(path = "R",
                        pattern = ".R$",
                        full.names = TRUE)

map(.x = functions,
    .f = source)


#################################################################################
#                       Tendances globales I2M2                                                    #
#################################################################################


#On calcule les tendances pour l'indice et les métriques de l'I2M2
Tendances_multi <-mk_st_by_group(clean_minv,resultat_indice,code_indice,code_indice,code_station_hydrobio)
Tendance_i2m2 <- filter(Tendances_multi, code_indice==7613) %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_ASPT <-filter(Tendances_multi,code_indice==8057)%>%select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_OVI <-filter(Tendances_multi,code_indice==8055)%>%select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_POL <-filter(Tendances_multi,code_indice==8056)%>%select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_SHA <-filter(Tendances_multi,code_indice==8058)%>%select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_RIC <-filter(Tendances_multi,code_indice==8054)%>%select(code_station_hydrobio,trend,sens_slope,mk_pvalue)

#----Tendances remarquables par métriques----
## I2M2 
# On garde seulement l'I2M2
i2m2 <- filter(clean_minv,code_indice==7613)

# On joint le jeu de données I2M2 à celui où la tendance à été calculé
i2m2_et_trend <- left_join(i2m2,Tendance_i2m2,by="code_station_hydrobio")

# On charge les délimitations des départements bretons grâce au package COGITER
departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)

# Si la tendance est croissante et significative : triangle vert; si elle est
#décroissante : triangle rouge
i2m2_et_trend <- i2m2_et_trend %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      
      trend == "Decrease" ~ "\u25BC"
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
      
    )
  )

# Transformation en objet sf
i2m2_et_trend_sf <- st_as_sf(i2m2_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = i2m2_et_trend_sf) +
  geom_sf_text(
    data = i2m2_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : Amélioration, ▼ : Dégradation, ● : Aucun changement")


## Tendance ASPT 
ASPT <- filter(clean_minv,code_indice==8057)
aspt_et_trend <- left_join(ASPT,Tendance_ASPT,by="code_station_hydrobio")

# Si la tendance est croissante et significative : triangle vert; si elle est
#décroissante : triangle rouge
aspt_et_trend <- aspt_et_trend %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      
      trend == "Decrease" ~ "\u25BC"
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
      
    )
  )

# Transformation en objet sf
aspt_et_trend_sf <- st_as_sf(aspt_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = aspt_et_trend_sf) +
  geom_sf_text(
    data = aspt_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : Amélioration, ▼ : Dégradation, ● : Aucun changement")


## Ovoviviparité
OVI <- filter(clean_minv,code_indice==8055)
ovi_et_trend <- left_join(OVI,Tendance_OVI,by="code_station_hydrobio")

ovi_et_trend <- ovi_et_trend %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      
      trend == "Decrease" ~ "\u25BC"
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
      
    )
  )

ovi_et_trend_sf <- st_as_sf(ovi_et_trend, coords = c("longitude", "latitude"), crs = 4326 )


departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)


# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ovi_et_trend_sf) +
  geom_sf_text(
    data = ovi_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : Amélioration, ▼ : Dégradation, ● : Aucun changement")


## Polyvoltinisme 
POL <- filter(clean_minv,code_indice==8056)
pol_et_trend <- left_join(POL,Tendance_POL,by="code_station_hydrobio")

pol_et_trend <- pol_et_trend %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      
      trend == "Decrease" ~ "\u25BC"
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
      
    )
  )

pol_et_trend_sf <- st_as_sf(pol_et_trend, coords = c("longitude", "latitude"), crs = 4326 )


departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)


# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = pol_et_trend_sf) +
  geom_sf_text(
    data = pol_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : Amélioration, ▼ : Dégradation, ● : Aucun changement")


## Indice de Shannon
SHA <- filter(clean_minv,code_indice==8058)
sha_et_trend <- left_join(SHA,Tendance_SHA,by="code_station_hydrobio")

sha_et_trend_sf <- st_as_sf(sha_et_trend, coords = c("longitude", "latitude"), crs = 4326 )


departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)


# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = sha_et_trend_sf) +
  geom_sf_text(
    data = sha_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : Amélioration, ▼ : Dégradation, ● : Aucun changement")

## Richesse taxonomique 
RIC <- filter(clean_minv,code_indice==8054)
ric_et_trend <- left_join(RIC,Tendance_RIC,by="code_station_hydrobio")

ric_et_trend <- ric_et_trend %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      
      trend == "Decrease" ~ "\u25BC"
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
      
    )
  )

ric_et_trend_sf <- st_as_sf(ric_et_trend, coords = c("longitude", "latitude"), crs = 4326 )


departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)


# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ric_et_trend_sf) +
  geom_sf_text(
    data = ric_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : Amélioration, ▼ : Dégradation, ● : Aucun changement")


# Cartographie intéractive avec mapview des indices et métriques

mapview(i2m2_et_trend, xcol="longitude", ycol="latitude", zcol="trend", crs= 4326, grid=FALSE, layer.name="Evolution I2M2")
mapview(aspt_et_trend,xcol="longitude", ycol="latitude", zcol="trend", crs= 4326, grid=FALSE, layer.name="Evolution")
mapview(ovi_et_trend,xcol="longitude", ycol="latitude", zcol="trend", crs= 4326, grid=FALSE, layer.name="Evolution")
mapview(pol_et_trend,xcol="longitude", ycol="latitude", zcol="trend", crs= 4326, grid=FALSE, layer.name="Evolution")
mapview(sha_et_trend,xcol="longitude", ycol="latitude", zcol="trend", crs= 4326, grid=FALSE, layer.name="Evolution")
mapview(ric_et_trend,xcol="longitude", ycol="latitude", zcol="trend", crs= 4326, grid=FALSE, layer.name="Evolution")

#################################################################################
#                       Tendances à la baisse I2M2  et ses métriques                                                  #
#################################################################################

# Tendance à la baisse I2M2
ggplot(filter(i2m2_et_trend,str_detect(trend,'Decrease'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#Tendance à la baisse ASPT
ggplot(filter(aspt_et_trend,str_detect(trend,'Decrease'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

# Tendancde à la baisse Ovoviviparité
ggplot(filter(ovi_et_trend,str_detect(trend,'Decrease'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

# Tendance à la baisse polyvoltinisme 
ggplot(filter(pol_et_trend,str_detect(trend,'Decrease'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

# Tendance à la baisse indice de Shannon
ggplot(filter(sha_et_trend,str_detect(trend,'Decrease'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

# Tendance à la baisse richesse taxonomique 
ggplot(filter(ric_et_trend,str_detect(trend,'Decrease'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#################################################################################
#                       Tendances à la hausse I2M2  et ses métriques                                                  #
#################################################################################

#Tendance à la hausse I2M2
ggplot(filter(i2m2_et_trend,str_detect(trend,'Increase'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +    theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                            axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#Tendance à la hausse ASPT
ggplot(filter(aspt_et_trend,str_detect(trend,'Increase'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +    theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                            axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#Tendance à la hausse indice de Shannon
ggplot(filter(sha_et_trend,str_detect(trend,'Increase'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +    theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                            axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#Tendance à la hausse ovoviviparité
ggplot(filter(ovi_et_trend,str_detect(trend,'Increase'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +    theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                            axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#Tendance à la hausse polyvoltinisme 
ggplot(filter(pol_et_trend,str_detect(trend,'Increase'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +    theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                            axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#Tendance à la hausse richesse taxonomique 
ggplot(filter(ric_et_trend,str_detect(trend,'Increase'))) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +    theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                            axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

################################################################################
# Traçage de l'évolution de l'I2M2 et métriques par département
################################################################################

# On dimmensionne le graphique

nbcol <- 5#nb de collonnes du facet"

tailleligne <- 10 #tres empirique !

# On crée les df associcés 
# I2M2
i2m2_22 <- filter(filter(clean_minv,code_indice==7613),code_departement==22)
nbsta <- length(unique(i2m2_22$code_station_hydrobio))
hauteurfacet22 <-  ceiling(nbsta/nbcol)*tailleligne

i2m2_29 <- filter(filter(clean_minv,code_indice==7613),code_departement==29)
nbsta <- length(unique(i2m2_29$code_station_hydrobio))
hauteurfacet29 <-  ceiling(nbsta/nbcol)*tailleligne

i2m2_35 <- filter(filter(clean_minv,code_indice==7613),code_departement==35)
nbsta <- length(unique(i2m2_35$code_station_hydrobio))
hauteurfacet35 <-  ceiling(nbsta/nbcol)*tailleligne

i2m2_56 <- filter(filter(clean_minv,code_indice==7613),code_departement==56)
nbsta <- length(unique(i2m2_56$code_station_hydrobio))
hauteurfacet56 <-  ceiling(nbsta/nbcol)*tailleligne

# Graphiques 
ggplot(i2m2_22) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   
  theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))
ggplot(i2m2_29) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   
  theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))
ggplot(i2m2_35) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   
  theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))
ggplot(i2m2_56) +  geom_point(aes(x = annee, color=libelle_station_hydrobio,y =resultat_indice,legend="false"))+geom_line(aes(x = annee, y =resultat_indice))+ facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y") + theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                                                                                                                                                                                     axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#On fait pareil mais pour les autres indices : ASPT
aspt_22 <- filter(filter(clean_minv,code_indice==8057),code_departement==22)
nbsta <- length(unique(aspt_22$code_station_hydrobio))
hauteurfacet22 <-  ceiling(nbsta/nbcol)*tailleligne

aspt_29 <- filter(filter(clean_minv,code_indice==8057),code_departement==29)
nbsta <- length(unique(aspt_29$code_station_hydrobio))
hauteurfacet29 <-  ceiling(nbsta/nbcol)*tailleligne

aspt_35 <- filter(filter(clean_minv,code_indice==8057),code_departement==35)
nbsta <- length(unique(aspt_35$code_station_hydrobio))
hauteurfacet35 <-  ceiling(nbsta/nbcol)*tailleligne

aspt_56 <- filter(filter(clean_minv,code_indice==8057),code_departement==56)
nbsta <- length(unique(aspt_56$code_station_hydrobio))
hauteurfacet56 <-  ceiling(nbsta/nbcol)*tailleligne

# Graphiques 
ggplot(aspt_22) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(aspt_29) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(aspt_35) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(aspt_56) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))


#Oviviparité :
ovi_22 <- filter(filter(Multi_indice_minv_s,code_indice==8055),code_departement==22)
nbsta <- length(unique(ovi_22$code_station_hydrobio))
hauteurfacet22 <-  ceiling(nbsta/nbcol)*tailleligne

ovi_29 <- filter(filter(Multi_indice_minv_s,code_indice==8055),code_departement==29)
nbsta <- length(unique(ovi_29$code_station_hydrobio))
hauteurfacet29 <-  ceiling(nbsta/nbcol)*tailleligne

ovi_35 <- filter(filter(Multi_indice_minv_s,code_indice==8055),code_departement==35)
nbsta <- length(unique(ovi_35$code_station_hydrobio))
hauteurfacet35 <-  ceiling(nbsta/nbcol)*tailleligne

ovi_56 <- filter(filter(Multi_indice_minv_s,code_indice==8055),code_departement==56)
nbsta <- length(unique(ovi_56$code_station_hydrobio))
hauteurfacet56 <-  ceiling(nbsta/nbcol)*tailleligne

# Graphiques 
ggplot(ovi_22) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(ovi_29) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(ovi_35) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(ovi_56) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#Polyvoltinisme :
pol_22 <- filter(filter(Multi_indice_minv_s,code_indice==8056),code_departement==22)
nbsta <- length(unique(pol_22$code_station_hydrobio))
hauteurfacet22 <-  ceiling(nbsta/nbcol)*tailleligne

pol_29 <- filter(filter(Multi_indice_minv_s,code_indice==8056),code_departement==29)
nbsta <- length(unique(pol_29$code_station_hydrobio))
hauteurfacet29 <-  ceiling(nbsta/nbcol)*tailleligne

pol_35 <- filter(filter(Multi_indice_minv_s,code_indice==8056),code_departement==35)
nbsta <- length(unique(pol_35$code_station_hydrobio))
hauteurfacet35 <-  ceiling(nbsta/nbcol)*tailleligne

pol_56 <- filter(filter(Multi_indice_minv_s,code_indice==8056),code_departement==56)
nbsta <- length(unique(pol_56$code_station_hydrobio))
hauteurfacet56 <-  ceiling(nbsta/nbcol)*tailleligne

#Graphiques 
ggplot(pol_22) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(pol_29) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(pol_35) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(pol_56) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))


#Diversité de Shannon :
sha_22 <- filter(filter(Multi_indice_minv_s,code_indice==8058),code_departement==22)
nbsta <- length(unique(sha_22$code_station_hydrobio))
hauteurfacet22 <-  ceiling(nbsta/nbcol)*tailleligne

sha_29 <- filter(filter(Multi_indice_minv_s,code_indice==8058),code_departement==29)
nbsta <- length(unique(sha_29$code_station_hydrobio))
hauteurfacet29 <-  ceiling(nbsta/nbcol)*tailleligne

sha_35 <- filter(filter(Multi_indice_minv_s,code_indice==8058),code_departement==35)
nbsta <- length(unique(sha_35$code_station_hydrobio))
hauteurfacet35 <-  ceiling(nbsta/nbcol)*tailleligne

sha_56 <- filter(filter(Multi_indice_minv_s,code_indice==8058),code_departement==56)
nbsta <- length(unique(sha_56$code_station_hydrobio))
hauteurfacet56 <-  ceiling(nbsta/nbcol)*tailleligne

# Graphiques 
ggplot(sha_22) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(sha_29) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(sha_35) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(sha_56) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))


#Richesse taxonomique :
ric_22 <- filter(filter(Multi_indice_minv_s,code_indice==8058),code_departement==22)
nbsta <- length(unique(ric_22$code_station_hydrobio))
hauteurfacet22 <-  ceiling(nbsta/nbcol)*tailleligne

ric_29 <- filter(filter(Multi_indice_minv_s,code_indice==8058),code_departement==29)
nbsta <- length(unique(ric_29$code_station_hydrobio))
hauteurfacet29 <-  ceiling(nbsta/nbcol)*tailleligne

ric_35 <- filter(filter(Multi_indice_minv_s,code_indice==8058),code_departement==35)
nbsta <- length(unique(ric_35$code_station_hydrobio))
hauteurfacet35 <-  ceiling(nbsta/nbcol)*tailleligne

ric_56 <- filter(filter(Multi_indice_minv_s,code_indice==8058),code_departement==56)
nbsta <- length(unique(ric_56$code_station_hydrobio))
hauteurfacet56 <-  ceiling(nbsta/nbcol)*tailleligne

# Graphiques 
ggplot(ric_22) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(ric_29) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(ric_35) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(ric_56) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

#################################################################################
#                               IBD                                                 #
#################################################################################


#On calcule les tendances
Tendances_donneesdiat<-mk_st_by_group(clean_ibd,resultat_indice,code_indice,code_indice,code_station_hydrobio)
Tendance_ibd <- filter(Tendances_donneesdiat, code_indice==5856) %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_ips <-filter(Tendances_donneesdiat,code_indice==1022)%>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)

ibd <- filter(clean_ibd,code_indice==5856)
ibd_et_trend <- left_join(ibd,Tendance_ibd,by="code_station_hydrobio")

ips <- filter(clean_ibd, code_indice==1022)
ips_et_trend <- left_join(ips, Tendance_ips, by="code_station_hydrobio")


# On cartographie 

mapview(ibd_et_trend, xcol="longitude", ycol="latitude", zcol="trend", crs= 4326, grid=FALSE, layer.name="Evolution")
mapview(ips_et_trend,xcol="longitude", ycol="latitude", zcol="trend", crs= 4326, grid=FALSE, layer.name="Evolution")

# On dimmensionne le graphique
nbcol <- 5#nb de collonnes du facet"

tailleligne <- 10 #tres empirique !

#On crée les df associés

ibd_22 <- filter(filter(clean_ibd,code_indice==5856),code_departement==22)
nbsta <- length(unique(ibd_22$code_station_hydrobio))
hauteurfacet22 <-  ceiling(nbsta/nbcol)*tailleligne

ibd_29 <- filter(filter(clean_ibd,code_indice==5856),code_departement==29)
nbsta <- length(unique(ibd_29$code_station_hydrobio))
hauteurfacet29 <-  ceiling(nbsta/nbcol)*tailleligne

ibd_35 <- filter(filter(clean_ibd,code_indice==5856),code_departement==35)
nbsta <- length(unique(ibd_35$code_station_hydrobio))
hauteurfacet35 <-  ceiling(nbsta/nbcol)*tailleligne

ibd_56 <- filter(filter(clean_ibd,code_indice==5856),code_departement==56)
nbsta <- length(unique(ibd_56$code_station_hydrobio))
hauteurfacet56 <-  ceiling(nbsta/nbcol)*tailleligne

ggplot(ibd_22) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(ibd_29) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(ibd_35) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))

ggplot(ibd_56) +   geom_point(aes(x = annee, y =resultat_indice,color=libelle_station_hydrobio,show.legend="false"))+
  geom_line(aes(x = annee, y =resultat_indice))+
  facet_wrap(~libelle_station_hydrobio,ncol = 3,scale="free_y")  +   theme(strip.text.x = element_text(size = 6, colour ="black"),
                                                                           axis.text.x = element_text(angle = 45,hjust=1))+
  theme(legend.position = "none")+coord_cartesian(ylim=c(0,1))


ips_22 <- filter(filter(clean_ibd,code_indice==5856),code_departement==22)
nbsta <- length(unique(ips_22$code_station_hydrobio))
hauteurfacet22 <-  ceiling(nbsta/nbcol)*tailleligne

ips_29 <- filter(filter(clean_ibd,code_indice==5856),code_departement==29)
nbsta <- length(unique(ips_29$code_station_hydrobio))
hauteurfacet29 <-  ceiling(nbsta/nbcol)*tailleligne

ips_35 <- filter(filter(clean_ibd,code_indice==5856),code_departement==35)
nbsta <- length(unique(ips_35$code_station_hydrobio))
hauteurfacet35 <-  ceiling(nbsta/nbcol)*tailleligne

ips_56 <- filter(filter(clean_ibd,code_indice==5856),code_departement==56)
nbsta <- length(unique(ips_56$code_station_hydrobio))
hauteurfacet56 <-  ceiling(nbsta/nbcol)*tailleligne

save(i2m2,
     ibd,
     Tendance_i2m2,
     Tendance_ASPT,
     Tendance_OVI,
     Tendance_POL,
     Tendance_RIC,
     Tendance_SHA,
     Tendance_ibd,
     Tendance_ips,
     file = "Data/50_chroniques_et_tendance.rda")




