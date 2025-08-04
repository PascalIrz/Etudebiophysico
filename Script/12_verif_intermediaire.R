# Chargement des df et des librairies 
load(file = "Data/10_donnees_pretraitees.rda")
library(tidyverse)
library(readxl)
library(ggplot2)

# Créer un tableau pour comparer le nombre d'année de données pour chacun des indices

# Tableau ou on compte le nombre de prélèvement par station IBD
ibd_counts <- clean_ibd %>% 
  filter(code_indice==5856) %>% 
  group_by(code_station_hydrobio) %>% 
  summarise(IBD = n(), .groups = "drop")

# Tableau ou on compte le nombre de prélèvement par station I2M2
i2M2_counts <- clean_minv %>% 
  filter(code_indice==7613) %>% 
  group_by(code_station_hydrobio) %>% 
  summarise(I2M2 = n(), .groups = "drop")

# Joindre les deux tableaux 
stations_indice_counts <- full_join(ibd_counts, i2M2_counts, by = "code_station_hydrobio") %>% 
  replace_na(list(IBD = 0, I2M2 = 0))
stations_indice_counts <- stations_indice_counts %>% 
  left_join(clean_ibd %>% select(code_station_hydrobio, libelle_station_hydrobio) %>% distinct(),
            by= "code_station_hydrobio")


# Créer un tableau pour compter le nombre RCO, RRP...

# On prépare les données
station_reseau <- read_excel("C:/Users/ilona.garcia/Documents/RstudioGIT/Etudebiophysico/Data/reseau_stations.xlsx")

# Jointure par le code_station_hydrobio
minv_joint <- clean_minv %>% 
  left_join(station_reseau, by = "code_station_hydrobio")

# Jointure par le code_station_hydrobio, on regarde quelles sont 
#les stations sans reseau
stations_sans_reseau <- clean_minv %>% 
  left_join(station_reseau, by = "code_station_hydrobio") %>% 
  filter(is.na(reseau)) %>% 
  distinct(code_station_hydrobio, code_departement)

# Jointure par le code_station_hydrobio
ibd_joint <- clean_ibd %>% 
  left_join(station_reseau, by = "code_station_hydrobio")

# Jointure par le code_station_hydrobio, on regarde quelles sont les stations
#sans réseau
stations_sans_reseau_ibd <- clean_ibd %>% 
  left_join(station_reseau, by = "code_station_hydrobio") %>% 
  filter(is.na(reseau)) %>% 
  distinct(code_station_hydrobio, code_departement)

# On calcule

# Proportion de stations pour chaque réseau
proportion_i2m2 <- minv_joint %>% 
  distinct(code_station_hydrobio,reseau) %>% 
  count(reseau) %>% 
  mutate(
    proportion = n/sum(n),
    code_indice = "7613"
  )

# Proportion de stations pour chaque réseau
proportion_ibd <- ibd_joint %>% 
  distinct(code_station_hydrobio,reseau) %>% 
  count(reseau) %>% 
  mutate(
    proportion = n/sum(n),
    code_indice = "5856"
  )

# Mise en commun des df
proportion_reseau <- bind_rows(proportion_i2m2, proportion_ibd) %>% 
  mutate(
    pourcentage = round(proportion * 100, 1),
    code_indice = recode(code_indice, "7613" = "I2M2", "5856" = "IBD")
  ) %>% 
  select(code_indice, reseau, n, proportion, pourcentage)

proportion_reseau <- proportion_reseau %>% 
  mutate(
    reseau = factor(reseau, levels = c("RRP", "RCS", "RCO", "RD", "NA"))
  )

# Graphique 
ggplot(proportion_reseau, aes(x = code_indice, y = reseau, fill = pourcentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(pourcentage, "%")), color = "black", size = 4) +
  scale_fill_gradient(low = "white", "high" = "darkgreen") +
  labs(
    title = "Pourcentage de stations par réseau et par indice",
    x = "Indice",
    y = "Réseau",
    fill = "Pourcentage"
  ) +
  scale_y_discrete(limits = rev) +
  theme_minimal()

