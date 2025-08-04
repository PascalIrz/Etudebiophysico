# Chargement df et librairies

library(tidyverse)
library(ggplot2)
load(file = "Data/10_donnees_pretraitees.rda")


#################################################################################
#                       I2M2                               #
#################################################################################

# On rentre les classes 

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

# Distribution des variables

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


#On fait un test log

ggplot(clean_minv, aes(x = resultat_indice)) +
  geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +
  facet_wrap(~ libelle_indice, scales = "free") +  # Un histogramme par métrique
  scale_x_log10() +
labs(title = "Distribution des valeurs des métriques de l'I2M2",
     x = "Valeur de la métrique",
     y = "Fréquence") +
  theme_minimal()


# Box plot

# On compte le nombre de prélèvement par année
station_annee <- clean_minv %>% 
  group_by(annee) %>% 
  summarise(n = n_distinct(code_station_hydrobio))
# Couleurs de l'arrière plan 
i2m2_class_boundaries <- data.frame(
  classe = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon"),
  ymin = c(-Inf, 0.148, 0.295, 0.433, 0.665),
  ymax = c(0.148, 0.295, 0.433, 0.665, Inf)
)

class_colors_map <- c(
  "Très bon" = "blue",
  "Bon" = "lightgreen",
  "Moyen" = "yellow",
  "Médiocre" = "orange",
  "Mauvais" = "red"
)

# Dans le bon ordre 
i2m2_class_boundaries$classe <- factor(i2m2_class_boundaries$classe, levels = names(class_colors_map))

# Pour chacune des facet
all_indices <- unique(clean_minv$code_indice)
background_rects_data <- do.call(rbind, lapply(all_indices, function(idx) {
  data.frame(
    code_indice = idx,
    i2m2_class_boundaries
  )
}))

background_rects_data$code_indice <- factor(background_rects_data$code_indice, levels = levels(factor(clean_minv$code_indice)))

ggplot(clean_minv, aes(x = factor(annee), y = resultat_indice)) +
  geom_rect(
    data = background_rects_data,
    aes(ymin = ymin, ymax = ymax, fill = classe),
    xmin = -Inf, xmax = Inf, 
    inherit.aes = FALSE
  ) +
  geom_boxplot() +
  geom_text(data = station_annee,
            aes(x = as.factor(annee), y = 0, label = paste0("n=", n)),
            vjust = 0.5, size = 2.5, color = "black",
            inherit.aes = FALSE) + 
  facet_wrap(~ libelle_indice, scales = "free_y") +
  scale_fill_manual(values = class_colors_map, name = "Classe I2M2") +
  labs(x = "Année", y = "Valeur") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "bottom", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#################################################################################
#                       IBD                             #
#################################################################################

# Distribution des variables 
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

# On met les classes dans l'ordre (du meilleur au pire)
clean_ibd_distrib$classe_ibd <- factor(clean_ibd_distrib$classe_ibd,
                                       levels = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais"))

# Graphique des distributions
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

# Box plot
station_annee_ibd <- clean_ibd %>% 
  group_by(annee) %>% 
  summarise(n = n_distinct(code_station_hydrobio))

# Limites des classes
ibd_class_boundaries <- data.frame(
  classe = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon"),
  ymin = c(-Inf, 5.9, 10, 13.8, 16.4),
  ymax = c(5.9, 10, 13.8, 16.4, Inf)
)

# On définit les couleurs 
ibd_class_colors_map <- c(
  "Très bon" = "blue",
  "Bon" = "lightgreen",
  "Moyen" = "yellow",
  "Médiocre" = "orange",
  "Mauvais" = "red"
)

# On s'assure du bon ordre
ibd_class_boundaries$classe <- factor(ibd_class_boundaries$classe, levels = names(ibd_class_colors_map))

# Pour chaque indice 
all_ibd_indices <- unique(clean_ibd$code_indice)
background_rects_data_ibd <- do.call(rbind, lapply(all_ibd_indices, function(idx) {
  data.frame(
    code_indice = idx,
    ibd_class_boundaries
  )
}))

background_rects_data_ibd$code_indice <- factor(background_rects_data_ibd$code_indice, levels = levels(factor(clean_ibd$code_indice)))

# Graphique
ggplot(clean_ibd, aes(x = factor(annee), y = resultat_indice)) +
  geom_rect(
    data = background_rects_data_ibd,
    aes(ymin = ymin, ymax = ymax, fill = classe),
    xmin = -Inf, xmax = Inf,
    inherit.aes = FALSE
  ) +
  geom_boxplot() +
  facet_wrap(~ libelle_indice, scales = "free_y") +
  scale_fill_manual(values = ibd_class_colors_map, name = "Classe IBD/IPS") +
  labs(x = "Année", y = "Valeur") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Box plot des valeurs annuelles 
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

  
# Box plot: valeurs mensuelles 

# On calcule les valeurs par mois 
parametres_physico_mois <- parametres_physico %>% 
  group_by(code_parametre,mois) %>% 
  summarise(
    moy_mois=mean(resultat, na.rm =TRUE),
    med_mois=median(resultat, na.rm = TRUE),
    ecart_type_mois=sd(resultat, na.rm = TRUE),
    max_mois=max(resultat, na.rm =TRUE),
    min_mois=min(resultat, na.rm = TRUE)
  )

# Graphique, distribution des paramètres physico-chimiques mensuelle 
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

  