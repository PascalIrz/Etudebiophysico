library(ggplot2)
library(dplyr)
load(file = "Data/10_donnees_pretraitees.rda")

#################################################################################
#                       I2M2                               #
#################################################################################

#Préparation des données : présence/absence
data_presence <- clean_minv %>%
  filter(libelle_indice == "Indice Invertébrés Multimétrique (I2M2)") %>%
  group_by(code_station_hydrobio, annee) %>%
  summarise(presence = ifelse(n() > 0, 1, 0), .groups = "drop") # 1 si données, 0 sinon

#On joint les libellés des stations au dataframe
data_presence_complet <- data_presence %>%
  left_join(
    clean_minv %>%
      select(code_station_hydrobio, libelle_station_hydrobio) %>%
      distinct(),
    by = "code_station_hydrobio"
  )

#Création du graphique
ggplot(data_presence_complet,
       aes(
         x = as.factor(annee),
         y = factor(libelle_station_hydrobio),
         fill = as.factor(presence)
       )) +
  geom_tile(
    color = "black",
    size = 0.1,
    height = 1.2,
    width = 1
  ) +
  scale_fill_manual(values = c("0" = "grey", "1" = "blue"), name = "Présence") +
  labs(title = "Présence des données I2M2 par station et par année",
       x = "Année",
       y = "Station") +
  theme_minimal(base_size = 146) +
  theme_light(base_size = 5) +
  theme(
    panel.grid.major = element_line(color = "#ffffff", size = 0.1),
    panel.grid.minor = element_line(color = "#ffffff",size = 0.1),
    panel.background = element_rect(fill = "red"),
    legend.position = "bottom"
  ) +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
scale_x_discrete(expand = c(0.05, 0.05))

nb_stations <- clean_minv %>%
  distinct(code_station_hydrobio) %>%
  nrow()
print(nb_stations)


#################################################################################
#                       Physico                           #
#################################################################################


#Préparation des données : présence/absence
data_presence_pc <- parametres_physico %>%
  filter(code_parametre%in%code_pc) %>%
  group_by(code_station_hydrobio,annee,mois) %>%
  summarise(presence = ifelse(n() > 0, 1, 0), .groups = "drop") # 1 si données, 0 sinon

#On joint les libellés des stations au dataframe
data_presence_complet_pc <- data_presence_pc %>%
  left_join(
    parametres_physico %>%
      select(code_station_hydrobio, libelle_station) %>%
      distinct(),
    by = "code_station_hydrobio"
  )

#Création du graphique
ggplot(data_presence_complet_pc,
       aes(
         x = as.factor(annee),
         y = factor(code_station_hydrobio),
         fill = as.factor(presence)
       )) +
  geom_tile(
    color = "black",
    size = 0.1
  ) +
  scale_fill_manual(values = c("0" = "grey", "1" = "blue"), name = "Présence") +
  labs(title = "Présence des données par station et par année",
       x = "Année",
       y = "Station") +
  theme_minimal(base_size = 146) +
  theme_light(base_size = 5) +
  theme(
    panel.grid.major = element_line(color = "#ffffff", size = 0.1),
    panel.grid.minor = element_line(color = "#ffffff", size=0.1),
    panel.background = element_rect(fill = "red"),
    legend.position = "bottom"
  ) +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_x_discrete(expand = c(0.05, 0.05))


#IBD 
#Préparation des données : présence/absence
data_presence_ibd <- clean_ibd %>%
  filter(code_indice == "5856") %>%
  group_by(code_station_hydrobio, annee) %>%
  summarise(presence = ifelse(n() > 0, 1, 0), .groups = "drop") # 1 si données, 0 sinon

#On joint les libellés des stations au dataframe
data_presence_complet_ibd <- data_presence_ibd %>%
  left_join(
    clean_ibd %>%
      select(code_station_hydrobio, libelle_station_hydrobio) %>%
      distinct(),
    by = "code_station_hydrobio"
  )

#Création du graphique
ggplot(data_presence_complet_ibd,
       aes(
         x = as.factor(annee),
         y = factor(libelle_station_hydrobio),
         fill = as.factor(presence)
       )) +
  geom_tile(
    color = "black",
    size = 0.1,
    height = 1.2,
    width = 1
  ) +
  scale_fill_manual(values = c("0" = "grey", "1" = "blue"), name = "Présence") +
  labs(title = "Présence des données IBD par station et par année",
       x = "Année",
       y = "Station") +
  theme_minimal(base_size = 146) +
  theme_light(base_size = 5) +
  theme(
    panel.grid.major = element_line(color = "#ffffff", size = 0.1),
    panel.grid.minor = element_line(color = "#ffffff",size = 0.1),
    panel.background = element_rect(fill = "red"),
    legend.position = "bottom"
  ) +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_x_discrete(expand = c(0.05, 0.05))



