library(ggplot2)

# Si données présentes : TRUE
station_year <- liste_taxon_ajout %>%
  distinct(libelle_station_hydrobio, annee) %>%
  mutate(donnees = TRUE)

# Graphique avec heatmap
ggplot(station_year, aes(x = as.factor(annee), y = libelle_station_hydrobio)) +
  geom_tile(aes(fill = donnees), color = "white") +
  scale_fill_manual(
    values = c("TRUE" = "steelblue"),
    name = "Données présentes", 
    labels = NULL               
  ) +
  labs(
    x = "Année",
    y = "Station",
    title = "Présence de données par station au cours des années"
  ) +
  theme_minimal(base_size = 12) +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 7, hjust = 0),
    legend.key.height = unit(0.8, "cm"),
    legend.title = element_text(size = 11),
    legend.position = "right"
  )
