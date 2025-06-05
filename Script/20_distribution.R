library(tidyverse)

load(file = "Data/10_donnees_pretraitees.rda")


#################################################################################
#                       I2M2                               #
#################################################################################

#Création de l'histogramme : donnees I2M2 et métriques 
ggplot(clean_minv %>% 
         mutate(classe_I2M2 = case_when(
            resultat_indice > 0.665 ~ "Très bon",
            resultat_indice > 0.433 ~ "Bon",
            resultat_indice > 0.295 ~ "Moyen",
            resultat_indice >= 0.148 ~ "Médiocre",
            resultat_indice < 0.148  ~ "Mauvais" 
           
         )), 
       aes(x = resultat_indice, fill = classe_I2M2)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.7) + 
  scale_fill_manual(values = c(
    "Très bon" = "blue",
    "Bon"= "lightgreen",
    "Moyen"="yellow",
    "Médiocre"="orange",
    "Mauvais"="red"
  ))+
  facet_wrap(~ libelle_indice, scales = "free") +  # Un histogramme par métrique
  labs(title = "Distribution des valeurs des métriques de l'I2M2",
       x = "Valeur de la métrique",
       y = "Fréquence") +
  theme_minimal()

#On fait un test log 
ggplot(clean_minv, aes(x = resultat_indice)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +
  facet_wrap(~ libelle_indice, scales = "free") +  # Un histogramme par métrique
  scale_x_log10() +
labs(title = "Distribution des valeurs des métriques de l'I2M2",
     x = "Valeur de la métrique",
     y = "Fréquence") +
  theme_minimal()

# Test de la normalité


# Box plot
ggplot(clean_minv, aes (x=factor(annee), y = resultat_indice)) +
  geom_boxplot() +
  facet_wrap(~ code_indice, scales = "free_y") +
  labs(x = "Mois", y= "Valeur", title ="Distribution des métriques par année") +
  theme_bw()

#################################################################################
#                       IBD                             #
#################################################################################
#Création de l'histogramme : donnees IBD
ggplot(clean_ibd, aes(x = resultat_indice)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +
  facet_wrap(~ libelle_indice, scales = "free") +
  labs(title = "Distribution des valeurs des indices",
       x = "Valeur de l'indice",
       y = "Fréquence") +
  theme_minimal()


ggplot(clean_ibd, aes (x=factor(annee), y = resultat_indice)) +
  geom_boxplot() +
  facet_wrap(~ code_indice, scales = "free_y") +
  labs(x = "Annee", y= "Valeur", title ="Distribution des indices par années") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#################################################################################
#                       Physico-chimie                               #
#################################################################################

# Distribution non logarithmique
ggplot(parametres_physico, aes(x =resultat)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +  # Histogramme avec 30 bins
  facet_wrap(~ libelle_parametre, scales = "free") +
  labs(title = "Distribution des valeurs",
       x = "",
       y = "Fréquence") +
  theme_minimal() 

# Distribution logarithmique 

parametres_physico <- parametres_physico %>% 
  mutate(resultat_log = log10(resultat))

ggplot(parametres_physico, aes(x =resultat_log)) +
  geom_histogram(bins = 50, fill = "#0072B2", color = "white", alpha = 0.7) +  # Histogramme avec 30 bins
  facet_wrap(~ libelle_parametre, scales = "free") +
  labs(title = "Distribution des valeurs",
       x = "",
       y = "Fréquence") +
  theme_minimal() 


# Test arcsin(sqrt)

parametres_physico %>%
  filter(libelle_parametre == "Ammonium") %>%
  ggplot(aes(x = resultat_arcsin)) +
  geom_histogram(
    bins = 100,
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


ggplot(parametres_physico, aes (x=factor(mois), y = resultat)) +
  geom_boxplot() +
  facet_wrap(~ code_parametre, scales = "free_y") +
  labs(x = "Mois", y= "Valeur", title ="Distribution des parametres par mois") +
  theme_bw()

