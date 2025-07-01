library(ggplot2)

load("Data/liste_taxon_ajout.Rdata")
load("Data/df_taxo.rda")

ggplot(abondance_relative, aes(x = annee, y= abondance_rel)) +
  geom_boxplot() +
  facet_wrap(~ code_appel_taxon == "807", scales = "free") +
  labs(title = "Distribution des valeurs des indices",
       x = "Valeur de l'indice",
       y = "Fréquence") +
  theme_minimal()

