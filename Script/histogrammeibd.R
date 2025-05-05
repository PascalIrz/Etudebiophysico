library(ggplot2) 
library(dplyr)
library(tidyr)
library(corrplot)
library(ggforce)

#Création de l'histogramme : donnees bio (global)
ggplot(clean_ibd, aes(x = resultat_indice)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +
  facet_wrap(~ libelle_indice, scales = "free") +
  labs(title = "Distribution des valeurs des indices",
       x = "Valeur de l'indice",
       y = "Fréquence") +
  theme_minimal()


