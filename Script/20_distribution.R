rm(list = ls())
library(tidyverse)

load(file = "Data/10_donnees_pretraitees.rda")


#################################################################################
#                       I2M2                               #
#################################################################################

#Création de l'histogramme : donnees I2M2 et métriques 
data <- clean_minv %>%
  left_join(y = correspond_code_libelle, by = c("code_indice" = "code")) %>% 
  mutate(libelle_court = as.factor(libelle_court),
         libelle_court = fct_relevel(libelle_court, "I2M2")) #pour tjs avoir I2M2 en premier
  

graph <- data %>%
  ggplot(aes(x = resultat_indice)) +
  geom_histogram(
    bins = 30,
    fill = "#0072B2",
    color = "white",
    alpha = 0.7
  ) +  # Histogramme avec 30 bins
  facet_wrap( ~ libelle_court, scales = "free") +  # Un histogramme par métrique
  labs(title = "Distribution des valeurs des métriques de l'I2M2",
       x = "Valeur de la métrique",
       y = "Fréquence") +
  theme_minimal()

graph

#On fait un test log 
graph +
  scale_x_log10()

# Test de la normalité


# Box plot
data %>%
  ggplot(aes (x = factor(annee), y = resultat_indice)) +
  geom_boxplot() +
  facet_wrap( ~ libelle_court, scales = "free_y") +
  labs(x = "Année", y = "Valeur", title = "Distribution des métriques par année") +
  theme_bw()

#################################################################################
#                       IBD                             #
#################################################################################

data <- clean_ibd %>%
  left_join(y = correspond_code_libelle, by = c("code_indice" = "code")) %>% 
  mutate(libelle_court = as.factor(libelle_court),
         libelle_court = fct_relevel(libelle_court, "IBD")) #pour tjs avoir IBD en premier

#Création de l'histogramme : donnees IBD

ggplot(data, aes(x = resultat_indice)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +
  facet_wrap(~ libelle_court, scales = "free") +
  labs(title = "Distribution des valeurs des indices",
       x = "Valeur de l'indice",
       y = "Fréquence") +
  theme_minimal()

ggplot(data, aes (x = factor(annee), y = resultat_indice)) +
  geom_boxplot() +
  facet_wrap(~ libelle_court, scales = "free_y") +
  labs(x = "Annee", y= "Valeur", title ="Distribution des indices par années") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#################################################################################
#                       Physico-chimie                               #
#################################################################################
parametres_physico <- parametres_physico %>% 
  mutate(resultat_log = log10(resultat))

data <- parametres_physico %>%
  left_join(y = correspond_code_libelle, by = c("code_parametre" = "code")) %>% 
  mutate(libelle_court = as.factor(libelle_court),
         libelle_court = fct_relevel(libelle_court,
                                     "Temp", "Turbidité", "Cond", "DBO5", "C orga", "MES", "O2", "Sat O2", "pH"))

# Distribution non logarithmique
ggplot(data, aes(x =resultat)) +
  geom_histogram(bins = 15, fill = "#0072B2", color = "white", alpha = 0.7) +  # Histogramme avec 30 bins
  facet_wrap(~ libelle_court, scales = "free") +
  labs(title = "Distribution des valeurs",
       x = "",
       y = "Fréquence") +
  theme_minimal() 

# Distribution logarithmique 

ggplot(data, aes(x =resultat_log)) +
  geom_histogram(bins = 15, fill = "#0072B2", color = "white", alpha = 0.7) +  # Histogramme avec 30 bins
  facet_wrap(~ libelle_court, scales = "free") +
  labs(title = "Distribution des valeurs",
       x = "",
       y = "Fréquence") +
  theme_minimal() 


# Test arcsin(sqrt)

parametres_physico %>%
  filter(libelle_parametre == "Ammonium") %>%
  ggplot(aes(x = resultat_arcsin)) +
  geom_histogram(
    bins = 15,
    fill = "#0072B2",
    color = "white",
    alpha = 0.7
  ) +  
  # facet_wrap( ~ libelle_parametre, scales = "free") +
  labs(title = "Distribution des valeurs",
       x = "",
       y = "Fréquence") +
  theme_minimal() +
  scale_x_continuous(limits = c(NA, 1)) +
  scale_x_sqrt()

  
###########

parametres_physico_mois <- parametres_physico %>% 
  group_by(code_parametre,mois) %>% 
  summarise(
    moy_mois=mean(resultat, na.rm =TRUE),
    med_mois=median(resultat, na.rm = TRUE),
    ecart_type_mois=sd(resultat, na.rm = TRUE),
    max_mois=max(resultat, na.rm =TRUE),
    min_mois=min(resultat, na.rm = TRUE)
  )


ggplot(data, aes (x=factor(mois), y = resultat)) +
  geom_boxplot() +
  facet_wrap(~ libelle_court, scales = "free_y") +
  labs(x = "Mois", y= "Valeur", title ="Distribution des parametres par mois") +
  theme_bw()

