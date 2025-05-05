library(dplyr)

#----Exploration des données : préliminaires I2M2----

#Combien de stations ont un ou deux prélevement par annees (avant tri)
comptemultibis <- count(Multi_indice_minv,libelle_station_hydrobio,code_indice) %>%  filter(n<2) %>% filter(code_indice==7613) %>%   select("libelle_station_hydrobio")
print(comptemultibis)

#Nombre total d'analyses par année 
nb_analyses_par_annee<-clean_minv%>%
  group_by(annee)%>%
  summarise(nb_analyses=n(),.groups="drop")
print(nb_analyses_par_annee)

#nombre d'années par station
nb_annees_par_station <- clean_minv %>%
  group_by(code_station_hydrobio) %>%
  summarise(nb_annees = n_distinct(annee), .groups = "drop")

# Affichage du résultat
print(nb_annees_par_station)

# Comptage du nombre d'analyses (résultats) par année et station pour chaque indice
nb_analyses_par_annee_station <- clean_minv %>%
  group_by(code_station_hydrobio, annee, code_indice) %>%
  summarise(nb_analyses = n(), .groups = "drop")

# Ajouter une colonne pour savoir si l'indice a été calculé plus d'une fois
nb_analyses_par_annee_station <- nb_analyses_par_annee_station %>%
  mutate(multiple_analyses = if_else(nb_analyses > 1, "Oui", "Non"))

# Affichage rapide du tableau avec les informations importantes
# Affichage uniquement des stations avec plus d'une analyse
resultat <- nb_analyses_par_annee_station %>%
  filter(nb_analyses > 1)

# Afficher le résultat
print(resultat)

#on regarde s'il y a des doublons
count(clean_minv,code_station_hydrobio,code_indice,annee, resultat_indice) %>% filter(n>1 & code_indice=='7613')
#on voit que pour certains cours d'eau on a fait deux prélèvements par an, peut être qu'il y a eu un aléas ou un paramétre douteux à vérif

#résumé des valeurs
summary(clean_minv)

#On visualise les stations 
stations_map<- clean_minv %>%
  select(code_station_hydrobio,libelle_station_hydrobio,longitude,latitude) %>%
  distinct()
mapview(stations_map, xcol="longitude", ycol="latitude", crs=4326, grid= FALSE, layer.name="Stations",cex=4)






