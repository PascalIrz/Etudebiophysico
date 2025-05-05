library(FactoMineR)
library(devtools)
library(factoextra)
library(dplyr)
library(tidyverse)

load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/70_choix_parametre.rda")
load(file = "Data/81_donnees_globales_trans.rda")

label <- c(
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
  "1841" = "C organique",
  "7613" = "I2M2",
  "8054" = "RichesI2M2",
  "8055" = "OvovivI2M2",
  "8056" = "PolyvolI2M2",
  "8057" = "ASPT",
  "8058" = "H'"
)

 
#################################################################################
#                       I2M2 et IBD inter-annuelle                                                  #
#################################################################################

acp<- clean_minv %>% 
  select(code_station_hydrobio, date_prelevement, code_indice, resultat_indice) %>% 
  distinct() %>% 
  pivot_wider(names_from = "code_indice",
              values_from = "resultat_indice") %>% 
  
  select(`8058`:`7613`)

donnees_centrees_reduites<- scale(acp,center=TRUE,scale=TRUE)

resultat_acp<- PCA(donnees_centrees_reduites, graph=TRUE)
print(resultat_acp)
valeurspropres<- resultat_acp$eig
valeurspropres

barplot(valeurspropres[,2],names.arg=1:nrow(valeurspropres),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres),valeurspropres[,2],
      type="b", pch=19, col="red")
        

fviz_pca_var(resultat_acp,
             col.var="cos2",
             gradient.cols=c("lightgreen", "yellow","orange"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")
fviz_contrib(resultat_acp,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp,choice="var", axes=2, top = 10)

#################################################################################
#                       I2M2 inter-station                                            #
#################################################################################

i2m2_ibd <- i2m2_wide %>% 
  left_join(ibd_wide, by = "code_station_hydrobio") %>% 
  select(`7613`:`5856`)

donnees_centrees_reduites_bio <- scale(i2m2_ibd,center=TRUE,scale=TRUE)

resultat_acp_bio<- PCA(donnees_centrees_reduites_bio, graph=TRUE)
print(resultat_acp_bio)
valeurspropres_bio<- resultat_acp_bio$eig
valeurspropres_bio

label_bio <- c("7613" = "I2M2",
               "8054" = "RichesI2M2",
               "8055" = "OvovivI2M2",
               "8056" = "PolyvolI2M2",
               "8057" = "ASPT",
               "8058" = "H'",
               "5856" = "IBD",
               "1022" = "IPS"
  
)

old_names_bio <- rownames(resultat_acp_bio$var$coord)
new_names_bio <- old_names_bio
new_names_bio[old_names_bio %in% names(label_bio)] <- label_bio[old_names_bio[old_names_bio %in% names(label_bio)]]


barplot(valeurspropres_bio[,2],names.arg=1:nrow(valeurspropres_bio),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres_bio),valeurspropres_bio[,2],
      type="b", pch=19, col="red")

resultat_acp_bio$var$coord <- `rownames<-`(resultat_acp_bio$var$coord, new_names_bio)
resultat_acp_bio$var$contrib <- `rownames<-`(resultat_acp_bio$var$contrib, new_names_bio)
resultat_acp_bio$var$cos2 <- `rownames<-`(resultat_acp_bio$var$cos2, new_names_bio)
resultat_acp_bio$var$cor <- `rownames<-`(resultat_acp_bio$var$cor, new_names_bio)

fviz_pca_var(resultat_acp_bio,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")
fviz_contrib(resultat_acp_bio,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_bio,choice="var", axes=2, top = 10)


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
#                       Physico valeurs mensuelles                                                
#################################################################################

acp_pc<- mean_physico_periode %>%
  select(code_station_hydrobio, annee, mois, code_parametre, para_moy) %>% 
  distinct() %>% 
  pivot_wider(names_from = "code_parametre",
              values_from = "para_moy",
              values_fn = mean) %>% 
  ungroup() %>%
  select(`mois`:`1841`) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

donnees_centrees_reduites_pc<- scale(acp_pc,center=TRUE,scale=TRUE)

resultat_acp_physico<- PCA(donnees_centrees_reduites_pc, graph=TRUE)
print(resultat_acp_physico)
valeurspropres_pc<- resultat_acp_physico$eig
valeurspropres_pc

barplot(valeurspropres_pc[,2],names.arg=1:nrow(valeurspropres_pc),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres_pc),valeurspropres_pc[,2],
      type="b", pch=19, col="red")

fviz_pca_ind(resultat_acp_physico,
             label = "none",
             habillage = acp_pc$mois,
             addEllipses = TRUE,
             palette = "Dark2",
             repel = TRUE) +
  labs(title = "ACP des mois")
             
fviz_pca_var(resultat_acp_physico,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")
fviz_contrib(resultat_acp_physico,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_physico,choice="var", axes=2, top = 10)

#graph avec mois et les années avec cette ACP
#transfo log? et enlever les outliers (4* ec type) --> en amont du remplacement,
#des valeurs manquantes
#modélisation : 1 ligne par site --> refaire ACP dessus (peut-être pas du tout même resultats),
#différentes periode entre I2M2 et diat ?? --> généralités sur mon tableau (ex )

#################################################################################
#                       Physico : inter-stations                                               
#################################################################################

acp_physico <- physico_wide %>%
  select(`1295`:`1841`)

donnees_centrees_reduites_pc_2 <- scale(acp_physico,center=TRUE,scale=TRUE)
resultat_acp_physico_2<- PCA(donnees_centrees_reduites_pc_2, graph=TRUE)
print(resultat_acp_physico_2)
valeurspropres_pc_2<- resultat_acp_physico_2$eig
valeurspropres_pc_2

old_names <- rownames(resultat_acp_physico_2$var$coord)
new_names <- old_names
new_names[old_names %in% names(label_physico)] <- label_physico[old_names[old_names %in% names(label_physico)]]

resultat_acp_physico_2$var$cos2 <- `rownames<-`(resultat_acp_physico_2$var$cos2, new_names)


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

fviz_pca_var(resultat_acp_physico_2,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")


fviz_contrib(resultat_acp_physico,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_physico,choice="var", axes=2, top = 10)


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
  select(`1301`:`1339`)

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
fviz_contrib(resultat_acp_physico,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_physico,choice="var", axes=2, top = 10)

#################################################################################
#                       Physico ; par sous groupe                                            
#################################################################################

#Inter-stations : NUTRIMENTS

nutri_wide <- physico_wide %>% 
  select(c("1350","1295","1313","1339","1335"))

donnees_centrees_reduites_nutri <- scale(nutri_wide,center=TRUE,scale=TRUE)
resultat_acp_nutri<- PCA(donnees_centrees_reduites_nutri, graph=TRUE)
print(resultat_acp_nutri)
valeurspropres_nutri<- resultat_acp_nutri$eig
valeurspropres_nutri

barplot(valeurspropres_nutri[,2],names.arg=1:nrow(valeurspropres_nutri),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres_nutri),valeurspropres_nutri[,2],
      type="b", pch=19, col="red")


fviz_pca_var(resultat_acp_nutri,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")
fviz_contrib(resultat_acp_physico,choice="var", axes=1, top = 10)
fviz_contrib(resultat_acp_physico,choice="var", axes=2, top = 10)


# Inter-station : Oxygène et nitrates

oxy_wide <- physico_wide %>% 
  select(c("1311","1312","1340"))

donnees_centrees_reduites_oxy <- scale(oxy_wide,center=TRUE,scale=TRUE)
resultat_acp_oxy <- PCA(donnees_centrees_reduites_oxy, graph=TRUE)
print(resultat_acp_oxy)
valeurspropres_oxy <- resultat_acp_oxy$eig
valeurspropres_oxy

barplot(valeurspropres_oxy[,2],names.arg=1:nrow(valeurspropres_oxy),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres_oxy),valeurspropres_oxy[,2],
      type="b", pch=19, col="red")


fviz_pca_var(resultat_acp_oxy,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")


# Inter-stations : PHYSIQUE 

acp_physic_wide <- physico_wide %>% 
  select(c("1295","1305","1313"))

donnees_centrees_reduites_physic <- scale(acp_physic_wide,center=TRUE,scale=TRUE)
resultat_acp_physic <- PCA(donnees_centrees_reduites_physic, graph=TRUE)
print(resultat_acp_physic)
valeurspropres_physic <- resultat_acp_physic$eig
valeurspropres_physic

barplot(valeurspropres_physic[,2],names.arg=1:nrow(valeurspropres_physic),
        main="Pourcentage de la variance expliquée par chaque composante",
        xlab="Composantes principales",
        ylab="Pourcentage de variance expliquée",
        col="steelblue")
#Add connected line segments to the plot
lines(x=1:nrow(valeurspropres_physic),valeurspropres_physic[,2],
      type="b", pch=19, col="red")


fviz_pca_var(resultat_acp_physic,
             col.var="cos2",
             gradient.cols=c("deeppink", "maroon","navy"),
             repel=TRUE,
             title="Cercle de Corrélation des Variables")



#################################################################################
#                       Global                                                
#################################################################################


acp_global <- df_global %>% 
  select(`7613`:`1841`)

donnees_centrees_reduites_global<- scale(acp_global,center=TRUE,scale=TRUE)

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


save (
  label,
  i2m2_ibd,
  resultat_acp_physico_2,
  file = "Data/81_acp"
        )


  


