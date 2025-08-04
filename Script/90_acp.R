#Chargement des librairies et données
library(FactoMineR)
library(devtools)
library(factoextra)
library(tidyverse)
library(ggplot2)

load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/70_choix_parametre.rda")
load(file = "Data/80_donnees_globales_trans.rda")

# Table de correspondance
label_physico <- c(
  "1301" = "T°",
  "1302" = "pH",
  "1303" = "Conductiv",
  "1305" = "MES",
  "1311" = "O2 dissous",
  "1312" = "Satur.O2",
  "1313" = "DBO5",
  "1335" = "NH4+",
  "1339" = "NO2-",
  "1340" = "NO3-",
  "1350" = "P total",
  "1433" = "PO4-",
  "1295" = "Turbidité",
  "1841" = "C organique"
)

label_bio <- c("7613" = "I2M2",
               "8054" = "RichesI2M2",
               "8055" = "OvovivI2M2",
               "8056" = "PolyvolI2M2",
               "8057" = "ASPT",
               "8058" = "H'",
               "5856" = "IBD",
               "1022" = "IPS"
               
)

#################################################################################
#                       I2M2 inter-station                                            #
#################################################################################

#i2m2_ibd <- i2m2_wide %>% 
  #left_join(ibd_wide, by = "code_station_hydrobio") %>% 
  #dplyr::select(`7613`:`5856`)

# On garde seulement les valeurs numériques 
i2m2_wide <- i2m2_wide %>% 
  select(`7613`:`8058`)

# Les valeurs sont centrées-réduites
donnees_centrees_reduites_bio <- scale(i2m2_wide,center=TRUE,scale=TRUE)

# On fait l'ACP 
resultat_acp_bio<- PCA(donnees_centrees_reduites_bio, graph=TRUE)

# On récupère les valeurs propres
valeurspropres_bio<- resultat_acp_bio$eig

# On renomme
old_names_bio <- rownames(resultat_acp_bio$var$coord)
new_names_bio <- old_names_bio
new_names_bio[old_names_bio %in% names(label_bio)] <- label_bio[old_names_bio[old_names_bio %in% names(label_bio)]]

# Barplot
barplot(valeurspropres_bio[,2],names.arg=1:nrow(valeurspropres_bio),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
lines(x=1:nrow(valeurspropres_bio),valeurspropres_bio[,2],
      type="b", pch=19, col="red")

resultat_acp_bio$var$coord <- `rownames<-`(resultat_acp_bio$var$coord, new_names_bio)
resultat_acp_bio$var$contrib <- `rownames<-`(resultat_acp_bio$var$contrib, new_names_bio)
resultat_acp_bio$var$cos2 <- `rownames<-`(resultat_acp_bio$var$cos2, new_names_bio)
resultat_acp_bio$var$cor <- `rownames<-`(resultat_acp_bio$var$cor, new_names_bio)

# Visualisation de l'ACP
fviz_pca_var(resultat_acp_bio,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle des corrélations des variables")

# Contribution sur les différents axes
fviz_contrib(resultat_acp_bio,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_bio,choice="var", axes=2, top = 10)
fviz_contrib(resultat_acp_bio,choice="var", axes=3, top = 10)



#################################################################################
#                       Bio : on teste par année                                              
#################################################################################


clean_minv_2018 <- clean_minv %>% 
  filter(annee == 2018) %>% 
  select(code_station_hydrobio, code_indice, resultat_indice)

acp_i2m2_2018 <- clean_minv_2018 %>% 
  pivot_wider(names_from = code_indice,
              values_from = resultat_indice)
  

clean_ibd_2018 <- clean_ibd %>% 
  filter(annee == 2018) %>% 
  select(code_station_hydrobio, code_indice, resultat_indice)

acp_ibd_2018 <- clean_ibd_2018 %>% 
  pivot_wider(names_from = code_indice,
              values_from = resultat_indice) 

acp_bio_2018 <- acp_i2m2_2018 %>% 
  left_join(acp_ibd_2018, by = "code_station_hydrobio") %>% 
  select(`8054`:`1022` )

donnees_centrees_reduites_2018 <- scale(acp_bio_2018,center=TRUE,scale=TRUE)

resultat_acp_bio_2018<- PCA(donnees_centrees_reduites_2018, graph=TRUE)
print(resultat_acp_bio_2018)
valeurspropres_bio_2018<- resultat_acp_bio_2018$eig
valeurspropres_bio_2018

barplot(valeurspropres_bio_2018[,2],names.arg=1:nrow(valeurspropres_bio_2018),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres_bio_2018),valeurspropres_bio_2018[,2],
      type="b", pch=19, col="red")


fviz_pca_var(resultat_acp_bio_2018,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")
fviz_contrib(resultat_acp_bio,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_bio,choice="var", axes=2, top = 10)

#################################################################################
#                       Physico : inter-stations                                               
#################################################################################

# On garde seulement les variables numériques
acp_physico <- physico_wide %>%
  dplyr::select(`1295`:`1841`)

# On centre-réduit les variables
donnees_centrees_reduites_pc_2 <- scale(acp_physico,center=TRUE,scale=TRUE)

# ACP
resultat_acp_physico_2<- PCA(donnees_centrees_reduites_pc_2, graph=TRUE)

# On récupère les valeurs propres
valeurspropres_pc_2<- resultat_acp_physico_2$eig

# On renomme
old_names <- rownames(resultat_acp_physico_2$var$coord)
new_names <- old_names
new_names[old_names %in% names(label_physico)] <- label_physico[old_names[old_names %in% names(label_physico)]]


resultat_acp_physico_2$var$cos2 <- `rownames<-`(resultat_acp_physico_2$var$cos2, new_names)

# Barplot
barplot(valeurspropres_pc_2[,2],names.arg=1:nrow(valeurspropres_pc_2),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres_pc_2),valeurspropres_pc_2[,2],
      type="b", pch=19, col="red")

resultat_acp_physico_2$var$coord <- `rownames<-`(resultat_acp_physico_2$var$coord, new_names)
resultat_acp_physico_2$var$contrib <- `rownames<-`(resultat_acp_physico_2$var$contrib, new_names)
resultat_acp_physico_2$var$cos2 <- `rownames<-`(resultat_acp_physico_2$var$cos2, new_names)
resultat_acp_physico_2$var$cor <- `rownames<-`(resultat_acp_physico_2$var$cor, new_names)

# Visualisation de l'ACP
fviz_pca_var(resultat_acp_physico_2,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle des corrélations des variables")

# Contributions sur chacun des axes  
fviz_contrib(resultat_acp_physico_2,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_physico_2,choice="var", axes=2, top = 10)
fviz_contrib(resultat_acp_physico_2,choice="var", axes=3, top = 10)


#################################################################################
#                       Physico ; par année                                              
#################################################################################

mean_physico_periode_test <- mean_physico_periode %>% 
  filter(annee==2023) %>% 
  group_by(code_station_hydrobio, code_parametre) %>% 
  summarise(moy=mean(para_moy,na.rm = TRUE), .groups= "drop") 

acp_pc_test <- mean_physico_periode_test %>%
  pivot_wider(names_from = code_parametre,
              values_from = moy,
              values_fn = mean) %>% 
  dplyr::select(`1301`:`1339`)

donnees_centrees_reduites_pc_test <- scale(acp_pc_test,center=TRUE,scale=TRUE)
resultat_acp_physico_test<- PCA(donnees_centrees_reduites_pc_test, graph=TRUE)
print(resultat_acp_physico_test)
valeurspropres_pc_test<- resultat_acp_physico_test$eig
valeurspropres_pc_test

barplot(valeurspropres_pc_test[,2],names.arg=1:nrow(valeurspropres_pc_test),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres_pc_test),valeurspropres_pc_test[,2],
      type="b", pch=19, col="red")


fviz_pca_var(resultat_acp_physico_test,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")
fviz_contrib(resultat_acp_physico_test,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_physico_test,choice="var", axes=2, top = 10)
fviz_contrib(resultat_acp_physico_test,choice="var", axes=3, top = 10)


#################################################################################
#                       ACP sur le jeu de données global                                               
#################################################################################


acp_global <- df_global %>% 
  dplyr::select(`I2M2`:`Corga`)

donnees_centrees_reduites_global<-  scale(acp_global,center=TRUE,scale=TRUE)

resultat_acp_global<- PCA(donnees_centrees_reduites_global, graph=TRUE)
print(resultat_acp_global)
valeurspropres_global<- resultat_acp_global$eig
valeurspropres_global

old_names <- rownames(resultat_acp_global$var$coord)
new_names <- old_names
new_names[old_names %in% names(label)] <- label[old_names[old_names %in% names(label)]]


barplot(valeurspropres_global[,2],names.arg=1:nrow(valeurspropres_global),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres_global),valeurspropres_global[,2],
      type="b", pch=19, col="red")

resultat_acp_global$var$coord <- `rownames<-`(resultat_acp_global$var$coord, new_names)
resultat_acp_global$var$contrib <- `rownames<-`(resultat_acp_global$var$contrib, new_names)
resultat_acp_global$var$cos2 <- `rownames<-`(resultat_acp_global$var$cos2, new_names)
resultat_acp_global$var$cor <- `rownames<-`(resultat_acp_global$var$cor, new_names)

fviz_pca_var(resultat_acp_global,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")
fviz_contrib(resultat_acp_physico,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_physico,choice="var", axes=2, top = 10)


save(
  label,
  i2m2_ibd,
  resultat_acp_physico_2,
  file = "Data/90_acp.rda"
        )


  


