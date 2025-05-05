library(ggplot2) 
library(dplyr)
library(tidyr)
library(corrplot)
library(ggforce)

#Création de l'histogramme : donnees bio (global)
ggplot(clean_minv, aes(x = resultat_indice)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +  # Histogramme avec 30 bins
  facet_wrap(~ libelle_indice, scales = "free") +  # Un histogramme par métrique
  labs(title = "Distribution des valeurs des métriques de l'I2M2",
       x = "Valeur de la métrique",
       y = "Fréquence") +
  theme_minimal()

#On fait un test log 
ggplot(clean_minv, aes(x = resultat_indice)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +
  facet_wrap(~ libelle_indice, scales = "free") +  # Un histogramme par métrique
  scale_x_log10()
labs(title = "Distribution des valeurs des métriques de l'I2M2",
     x = "Valeur de la métrique",
     y = "Fréquence") +
  theme_minimal()

#Filtrer pour ne garder que l'année 2016
clean_minv_2016 <- clean_minv %>% filter(annee == 2016)

#Affichage des histogrammes pour chaque métrique
ggplot(clean_minv_2016, aes(x = resultat_indice)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "black", alpha = 0.7) +
  facet_wrap(~ libelle_indice, scales = "free") +
  scale_x_log10()
  labs(title = "Distribution des métriques pour l'année 2016",
       x = "Valeur des métriques",
       y = "Fréquence") +
  theme_minimal()

#Calcul de l'IQR pour chaque métrique
outliers_iqr <- clean_minv_2016 %>%
  group_by(libelle_indice) %>%
  mutate(Q1 = quantile(resultat_indice, 0.25, na.rm = TRUE),
         Q3 = quantile(resultat_indice, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         lower_bound = Q1 - 1.5 * IQR,
         upper_bound = Q3 + 1.5 * IQR) %>%
filter(resultat_indice < lower_bound | resultat_indice > upper_bound) %>%
select(code_station_hydrobio, libelle_indice, resultat_indice)  # On garde seulement les infos utiles
  
#Afficher les stations avec des valeurs aberrantes
print(outliers_iqr)
  
  
#On fait un test log 
ggplot(clean_minv, aes(x = resultat_indice)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +
  facet_wrap(~ libelle_indice, scales = "free") +  # Un histogramme par métrique
  scale_x_log10()
  labs(title = "Distribution des valeurs des métriques de l'I2M2",
       x = "Valeur de la métrique",
       y = "Fréquence") +
  theme_minimal()

shapiro_result<- apply(acp, 2, shapiro.test)
print(shapiro_result)
  

