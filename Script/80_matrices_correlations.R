library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)

load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/70_choix_parametre.rda")
load(file = "Data/81_donnees_globales_trans.rda")
load(file = "Data/81_acp_rda.rda")
#################################################################################
#                       I2M2 jeux de données gobal                                               #
#################################################################################

cor_matrix<- clean_minv %>% 
  select(code_station_hydrobio, date_prelevement, code_indice, resultat_indice) %>% 
  distinct() %>% 
  pivot_wider(names_from = "code_indice",
              values_from = "resultat_indice") %>% 
  select(`8058`:`7613`) %>% 
  cor(method="spearman")
corrplot.mixed(cor_matrix,upper="ellipse") 

#################################################################################
#                       I2M2 et IBD inter-stations                                              #
#################################################################################

i2m2_ibd_matrix <- i2m2_ibd %>% 
  cor(method="spearman")
corrplot.mixed(i2m2_ibd_matrix,upper="ellipse") 

  
#################################################################################
#                       Physico jeux de données global                                             #
#################################################################################


cor_matrix_pc <- mean_physico %>% 
  distinct() %>% 
  pivot_wider(names_from = "code_parametre",
              values_from = "para_moy",
              values_fn = mean) %>% 
  ungroup() %>% 
  select(`1295`:`1433`) %>% 
  cor(method="spearman", use = "pairwise")



# Corrplot avec ellipse en haut et regroupement

corrplot(cor_matrix_pc, 
         lower = "number",
         upper = "ellipse",
         order = "hclust",
         addrect = 3,
         rect.col = "black",
         rect.lwd = 2)


corrplot.mixed(cor_matrix_pc,
               upper="ellipse")
               


#################################################################################
#                       Physico : inter-stations                                              #
#################################################################################

physico_wide_matrix <- physico_wide %>% 
  select(`1295`:`1841`) %>%
  cor(method="spearman", use = "pairwise")

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
  "1841" = "C organique")

colnames(physico_wide_matrix) <- label_physico
rownames(physico_wide_matrix) <- label_physico
  
corrplot(physico_wide_matrix, 
         lower = "number",
         upper = "ellipse",
         order = "hclust",
         addrect = 2,
         rect.col = "black",
         rect.lwd = 2)

corrplot.mixed(physico_wide_matrix,
               upper="ellipse")


colnames(physico_wide_matrix) <- label

  
corrplot(physico_wide_matrix, 
         lower = "number",
         upper = "ellipse",
         order = "hclust",
         addrect = 2,
         rect.col = "black",
         rect.lwd = 2)

corrplot.mixed(physico_wide_matrix,
               upper="ellipse")



physico_wide_parametres <- physico_wide %>% 
  select(`1295`:`1841`)

ggpairs(physico_wide_parametres,
        lower = list(continuous = wrap("smooth", method = lm, se=FALSE)),
        diag=list(continuous = wrap("densityDiag")),
        upper=list(continuous = wrap("cor",method="spearman",size=4)))

df_global_sans_station <- df_global %>% 
  select(`7613`:`1841`)

ggpairs(df_global_sans_station,
        lower = list(continuous = wrap("smooth", method = lm, se=FALSE)),
        diag=list(continuous = wrap("densityDiag")),
        upper=list(continuous = wrap("cor",method="spearman",size=4)))
