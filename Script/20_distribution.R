library(tidyverse)

load(file = "Data/10_donnees_pretraitees.rda")


#################################################################################
#                       I2M2                               #
#################################################################################

##### Test
clean_minv_distrib <- clean_minv %>%
  mutate(
    classe_I2M2 = case_when(
      resultat_indice > 0.665 ~ "Très bon",
      resultat_indice > 0.433 ~ "Bon",
      resultat_indice > 0.295 ~ "Moyen",
      resultat_indice >= 0.148 ~ "Médiocre",
      resultat_indice < 0.148  ~ "Mauvais",
      TRUE ~ NA_character_ 
    )
  ) %>%
  mutate(
    classe_I2M2 = factor(classe_I2M2,
                         levels = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais"))
  )

ggplot(clean_minv_distrib, aes(x = resultat_indice, fill = classe_I2M2)) +
  geom_histogram(binwidth = 0.01, color = "black", alpha = 0.7, stat = "bin") +
  geom_vline(xintercept = c(0.148, 0.295, 0.433, 0.665),
             linetype = "dashed", color = "grey50", linewidth = 0.8) +
  
  scale_fill_manual(
    values = c(
      "Très bon" = "blue",
      "Bon"      = "lightgreen",
      "Moyen"    = "yellow",
      "Médiocre" = "orange",
      "Mauvais"  = "red"
    ),
    name = "Classe I2M2"
  ) +
  facet_wrap(~ libelle_indice, labeller = label_wrap_gen(width = 20), scales = "free") +
  labs(
    title = "Distribution des valeurs des métriques de l'I2M2 par classe de qualité",
    x = "Valeur de la métrique",
    y = "Fréquence des prélèvements"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )


##### Ancienne version :Création de l'histogramme : donnees I2M2 et métriques 
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

station_annee <- clean_minv %>% 
  group_by(annee) %>% 
  summarise(n = n_distinct(code_station_hydrobio))

ggplot(clean_minv, aes (x=factor(annee), y = resultat_indice)) +
  geom_boxplot() +
  geom_text(data = station_annee,
            aes(x = as.factor(annee), y=0, label = paste0("n=",n)),
            vjust = 0.5, size = 3, color = "blue") +
  facet_wrap(~ code_indice, scales = "free_y") +
  labs(x = "Année", y= "Valeur", title ="Distribution des métriques par année") +
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

#### Test
clean_ibd_distrib <- clean_ibd %>%
  mutate(
    classe_ibd = case_when(
      resultat_indice > 16.4 ~ "Très bon",
      resultat_indice > 13.8 ~ "Bon",
      resultat_indice > 10   ~ "Moyen",
      resultat_indice > 5.9  ~ "Médiocre",
      TRUE                   ~ "Mauvais"
    )
  )

clean_ibd_distrib$classe_ibd <- factor(clean_ibd_distrib$classe_ibd,
                                       levels = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais"))

ggplot(clean_ibd_distrib, aes(x = resultat_indice, fill = classe_ibd)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7, stat = "bin") +
  geom_vline(xintercept = c(5.9, 10, 13.8, 16.4),
             linetype = "dashed", color = "grey50", linewidth = 0.8) +
  
  scale_fill_manual(
    values = c(
      "Très bon" = "blue",
      "Bon"      = "lightgreen",
      "Moyen"    = "yellow",
      "Médiocre" = "orange",
      "Mauvais"  = "red"
    ),
    name = "Classe IBD"
  ) +
  facet_wrap(~ libelle_indice, labeller = label_wrap_gen(width = 20), scales = "free") +
  labs(
    title = "Distribution des valeurs des indices IBD et IPS par classe de qualité",
    x = "Valeur de l'indice",
    y = "Fréquence des prélèvements"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )         


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


donnees_freq <- parametres_physico %>% 
  mutate(
    annee = year(date_prelevement),
    mois = month(date_prelevement)
  ) %>% 
  group_by(code_station_hydrobio, annee, code_parametre) %>% 
  summarise (nb_mesures = n(), .groups = "drop")

  