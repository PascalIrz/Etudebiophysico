# Chargement des librairies et données
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)

load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/70_choix_parametre.rda")
load(file = "Data/80_donnees_globales_trans.rda")
load(file = "Data/90_acp.rda")

# Table de correspondance
label_bio <- c("7613" = "I2M2",
              "8054" = "RichesI2M2",
              "8055" = "OvovivI2M2",
              "8056" = "PolyvolI2M2",
              "8057" = "ASPT",
              "8058" = "H'",
              "5856" = "IBD",
              "1022" = "IPS"
              
)

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

#################################################################################
#                       I2M2 et IBD inter-stations                                              #
#################################################################################

# On calcule les corrélations avec le test de Spearman
i2m2_ibd_matrix <- i2m2_ibd %>% 
  cor(method="spearman")
corrplot.mixed(i2m2_ibd_matrix,upper="ellipse") 

# On renomme
colnames(i2m2_ibd_matrix) <- label_bio
rownames(i2m2_ibd_matrix) <- label_bio

# On corrige la taille de la police
corrplot.mixed(i2m2_ibd_matrix, upper = "ellipse", tl.cex = 0.48)

#################################################################################
#                       Physico : inter-stations                                              #
#################################################################################

# On garde seulement les variables physico-chimiques et on calcule les corrélations
# avec le test de corrélation de spearman
physico_wide_matrix <- physico_wide %>% 
  dplyr::select(`1295`:`1841`) %>%
  cor(method="spearman", use = "pairwise") 

# On renomme les variables
colnames(physico_wide_matrix) <- label_physico
rownames(physico_wide_matrix) <- label_physico

# On regarde s'il y a des groupes qui se distinguent 
corrplot(physico_wide_matrix, 
         lower = "number",
         upper = "ellipse",
         order = "hclust",
         addrect = 2,
         rect.col = "black",
         rect.lwd = 2)

# On corrige la taille de la police
corrplot.mixed(physico_wide_matrix,
               upper="ellipse",
               tl.cex = 0.45,
               tl.col = "black")

# On regarde les corrélations avec une autre visualisation : ggpairs
physico_wide_parametres <- physico_wide %>% 
  dplyr::select(`1295`:`1841`)

colnames(physico_wide_parametres) <- label_physico

ggpairs(physico_wide_parametres,
        lower = list(continuous = wrap("smooth", method = lm, se=FALSE)),
        diag=list(continuous = wrap("densityDiag")),
        upper=list(continuous = wrap("cor",method="spearman",size=4)))

# Corrélations entre variables physico et bio
df_global <- df_global %>% 
  rename(I2M2 = `7613`,
         richesI2M2 = `8054`,
         ovovivI2M2 = `8055`,
         polyvolI2M2 = `8056`,
         ASPT = `8057`,
         ShannonI2M2 = `8058`,
         IPS = `1022`,
         IBD = `5856`,
         turbidité = `1295`,
         Temp = `1301`,
         pH = `1302`,
         conductiv = `1303`,
         MES = `1305`,
         O2dissous = `1311`,
         SaturO2 = `1312`,
         DBO5 = `1313`,
         NH4 = `1335`,
         NO2 = `1339`,
         NO3 = `1340`,
         Ptot = `1350`,
         PO4 = `1433`,
         Corga = `1841`)

df_global_sans_station <- df_global %>% 
  dplyr::select(`I2M2`:`Corga`)

ggpairs(df_global_sans_station,
        lower = list(continuous = wrap("smooth", method = lm, se=FALSE)),
        diag=list(continuous = wrap("densityDiag")),
        upper=list(continuous = wrap("cor",method="spearman",size=4)))
