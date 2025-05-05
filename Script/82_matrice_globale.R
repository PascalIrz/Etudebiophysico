library(tidyverse)
library(ggplot2)
library(corrplot)

load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/70_choix_parametre.rda")


cor_matrix_global <- df_global %>% 
  select(`7613`:`1841`) %>% 
  cor(method="spearman")

corrplot.mixed(cor_matrix_global,upper="ellipse") 
