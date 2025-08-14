# Chargement df et librairie

library(tidyverse)
library(ggplot2)
library(mapview)
load(file = "Data/10_donnees_pretraitees.rda")

#################################################################################
#                       I2M2                               #
#################################################################################


#---- Statistique descriptive : variance----

## Calcul de la variance par année pour chaque indice
var_par_annee <- clean_minv %>%
  group_by(libelle_indice, annee) %>%
  summarise(var = var(resultat_indice, na.rm = TRUE), .groups = "drop")

# Graphique de la variance par année avec facet_wrap()
plot_var_annee <- ggplot(var_par_annee, aes(x = annee, y = var, color = libelle_indice, group = libelle_indice)) +
  geom_point() +
  geom_line() +
  facet_wrap(~libelle_indice, scales = "free_y") +
  theme_minimal() + coord_cartesian(ylim=c(0,0.25))
labs(title = "Variance des indices au fil des années", x = "Année", y = "Variance")

print(plot_var_annee)

### Calcul de la variance par station pour chaque indice
var_par_station <- clean_minv %>%
  group_by(libelle_indice, code_station_hydrobio, libelle_station_hydrobio) %>%
  summarise(var = var(resultat_indice, na.rm = TRUE), .groups = "drop")

# Graphique de la variance par station avec facet_wrap()
plot_var_station <- ggplot(var_par_station, aes(x = code_station_hydrobio, y = var, fill = libelle_indice)) +
  geom_col(position = "dodge") +
  facet_wrap(~libelle_indice, scales = "free_y") +
  theme_minimal() +coord_cartesian(ylim=c(0,0.15))

theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Variance des indices par station", x = "Station", y = "Variance")


#Calcul des quantiles des variances pour chaque indice
quantiles_variance <- var_par_station %>%
  group_by(libelle_indice) %>%
  summarise(
    Q1 = quantile(var, 0.25, na.rm = TRUE),
    Mediane = quantile(var, 0.50, na.rm = TRUE),
    Q3 = quantile(var, 0.75, na.rm = TRUE),
    Seuil_90 = quantile(var, 0.90, na.rm = TRUE)  # 90% des stations ont une variance inférieure à ce seuil
  )

# Ajout des quantiles à chaque station
var_par_station_quantiles <- var_par_station %>%
  inner_join(quantiles_variance, by = "libelle_indice")

# Sélection des 10% des stations les plus variables pour chaque indice
top10_var <- var_par_station_quantiles %>%
  filter(var >= Seuil_90)

# Vérification des résultats
table(top10_var$libelle_indice)  # Nombre de stations retenues par indice

# Visualisation des stations les plus variables par indice
ggplot(top10_var, aes(x = reorder(libelle_station_hydrobio, -var), y = var, fill = libelle_indice)) +
  geom_col() +
  facet_wrap(~libelle_indice, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 10% des stations les plus variables par indice",
       x = "Code Station",
       y = "Variance")

# Compter combien de fois chaque station est sélectionnée dans les top 10%
stations_counts <- top10_var %>%
  count(libelle_station_hydrobio, name = "nb_indices")

# Sélectionner les stations présentes dans au moins 2 indices différents
stations_communes <- top10_var %>%
  group_by(libelle_station_hydrobio) %>%
  summarise(
    nb_indices = n(),
    libelle_indice = paste(unique(libelle_indice), collapse = ", ")
  ) %>%
  filter(nb_indices >= 2)

# Récupérer leurs coordonnées
stations_communes_coords <- clean_minv %>%
  select(libelle_station_hydrobio, X = longitude, Y = latitude) %>%
  inner_join(stations_communes, by = "libelle_station_hydrobio")

# Conversion en objet sf pour la cartographie
stations_sf <- st_as_sf(stations_communes_coords, coords = c("X", "Y"), crs = 4326)
stations_sf <- stations_sf %>% mutate(libelle_indice = as.character(libelle_indice))

# Carte des stations les plus variables
mapview(stations_sf, zcol="nb_indices", col.regions=viridis::viridis)

#On garde seulement I2M2
top10_var_i2m2 <- top10_var %>%
  filter(libelle_indice == "Indice Invertébrés Multimétrique (I2M2)")

#Coordonnées géographiques
stations_coords_i2m2 <- clean_minv %>%
  filter(libelle_station_hydrobio %in% top10_var_i2m2$libelle_station_hydrobio) %>%
  select(libelle_station_hydrobio, X = longitude, Y = latitude) %>%
  distinct()

# Conversion en objet sf
stations_sf_i2m2 <- st_as_sf(stations_coords_i2m2, coords = c("X", "Y"), crs = 4326)

# Carte des stations les plus variables pour l'indice I2M2
mapview(stations_sf_i2m2)


#---- Statistique descriptive: médiane----

### Calcul de la médiane par année et par station

med_par_annee <- clean_minv %>%
  group_by(libelle_indice, annee) %>%
  summarise(med = median(resultat_indice, na.rm = TRUE),
            var=var(resultat_indice, na.rm = TRUE),
            ecart_type=sd(resultat_indice, na.rm = TRUE),
            .groups = "drop")


ggplot(med_par_annee, aes(x = annee, y = med, color = as.factor(libelle_indice))) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin= med - ecart_type, ymax= med + ecart_type),width=0.2) +
  facet_wrap(~libelle_indice, scales = "free_y") +
  labs(title = "Médiane des indices par année",
       x = "Année",
       y = "Médiane de l'indice",
       color = "Indice") +
  theme_minimal()+coord_cartesian(ylim=c(0,1))

# Calcul des médiannes par station
med_par_station <- clean_minv %>%
  group_by(libelle_indice, code_station_hydrobio) %>%
  summarise(med = median(resultat_indice, na.rm = TRUE), .groups = "drop")
ggplot(med_par_station, aes(x = code_station_hydrobio, y = med, color = as.factor(libelle_indice))) +
  geom_col() +
  geom_line() +
  facet_wrap(~libelle_indice, scales = "free_y") +
  labs(title = "Médiane des indices par station",
       x = "Station",
       y = "Médiane de l'indice",
       color = "Indice") +
  theme_minimal() +coord_cartesian(ylim=c(0,1))
theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Calcul des valeurs min en 2022, d'après le calcul des médianes
data_2022_i2m2 <- clean_minv %>%
  filter(libelle_indice == "Indice Invertébrés Multimétrique (I2M2)", annee == 2022)

#Sélectionner les 10 stations avec les valeurs les plus basses
stations_min_i2m2_2022 <- data_2022_i2m2 %>%
  arrange(resultat_indice) %>%
  slice_head(n = 10) %>%  
  select(libelle_station_hydrobio, code_station_hydrobio, resultat_indice)

#Afficher les résultats
print(stations_min_i2m2_2022)


#----Exploration stat : quartiles----
### Calcul des quantiles par année et par station
quantiles_par_annee <- clean_minv %>%
  group_by(libelle_indice, annee) %>%
  summarise(
    Q1 = quantile(resultat_indice, probs = 0.25, na.rm = TRUE),
    Q2 = quantile(resultat_indice, probs = 0.50, na.rm = TRUE),
    Q3 = quantile(resultat_indice, probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(quantiles_par_annee, aes(x = annee, color = as.factor(libelle_indice))) +
  geom_line(aes(y = Q1), linetype = "dashed") +
  geom_line(aes(y = Q2)) +
  geom_line(aes(y = Q3), linetype = "dotted") +
  facet_wrap(~libelle_indice, scales = "free_y") +
  labs(title = "Quantiles des indices par année",
       x = "Année",
       y = "Valeur de l'indice",
       color = "Indice") +
  theme_minimal()


#----Exploration stat : moyenne----

### Calcul de la moyenne par année et par station
moy_par_annee <- clean_minv %>%
  group_by(libelle_indice, annee) %>%
  summarise(moy = mean(resultat_indice, na.rm = TRUE), .groups = "drop")
ggplot(moy_par_annee, aes(x = annee, y = moy, color = as.factor(libelle_indice))) +
  geom_point() +
  geom_line() +
  facet_wrap(~libelle_indice, scales = "free_y") +
  labs(title = "Moyenne des indices par année",
       x = "Année",
       y = "Moyenne de l'indice",
       color = "Indice") +
  theme_minimal()+coord_cartesian(ylim=c(0,0.8))

# Calcul des moyennes par station
moy_par_station <- clean_minv %>%
  group_by(libelle_indice, code_station_hydrobio) %>%
  summarise(moy = mean(resultat_indice, na.rm = TRUE), .groups = "drop")
ggplot(moy_par_station, aes(x = code_station_hydrobio, y = moy, color = as.factor(libelle_indice))) +
  geom_col() +
  geom_line() +
  facet_wrap(~libelle_indice, scales = "free_y") +
  labs(title = "Moyenne des indices par station",
       x = "Station",
       y = "Moyenne de l'indice",
       color = "Indice") +
  theme_minimal()+coord_cartesian(ylim=c(0,1))


#calcul des moyennes

m_i2m2 <- filter(clean_minv,code_indice==7613) %>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))
print(m_i2m2)
m_aspt <- filter(clean_minv,code_indice==8057) %>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))
m_polyv <- filter(clean_minv,code_indice==8056)%>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))
m_ovov <- filter(clean_minv,code_indice==8055)%>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))
m_shannon <- filter(clean_minv,code_indice==8058)%>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))
m_rich <- filter(clean_minv,code_indice==8054)%>% 
  group_by(annee) %>% summarise(moy=mean(resultat_indice))

m_i2m2stat <- filter(clean_minv,code_indice==7613) %>% 
  group_by(code_station_hydrobio) %>% summarise(moy=mean(resultat_indice))
m_asptstat <- filter(clean_minv,code_indice==8057) %>% 
  group_by(code_station_hydrobio) %>% summarise(moy=mean(resultat_indice))
m_polyvstat <- filter(clean_minv,code_indice==8056)%>% 
  group_by(code_station_hydrobio) %>% summarise(moy=mean(resultat_indice))
m_ovovstat <- filter(clean_minv,code_indice==8055)%>% 
  group_by(code_station_hydrobio) %>% summarise(moy=mean(resultat_indice))
m_shannonstat <- filter(clean_minv,code_indice==8058)%>% 
  group_by(code_station_hydrobio) %>% summarise(moy=mean(resultat_indice))
m_richstat <- filter(clean_minv,code_indice==8054)%>% 
  group_by(code_station_hydrobio) %>% summarise(moy=mean(resultat_indice))



#chargement des seuils i2m2 +classes de qualité
# travail à faire ##############################
#load("../data/Classe_i2m2.Rda")#donne un DF Classe_i2m2
load("C:/Users/ilona.garcia/Documents/Rstudio/Hubeau_hydrobiologie/Data/classesi2m2.Rda")

gg_ajouter_arriere_plan_int <- function(graphique, classesi2m2) {
  graphique <- graphique + geom_rect(data = classesi2m2,aes(ymin = cli_borne_inf,ymax = cli_borne_sup,fill = cli_libelle),
                                     xmin = -Inf,
                                     xmax = Inf,
                                     alpha = 0.3) +
    scale_fill_manual(values = setNames(classesi2m2$classe_couleur,classesi2m2$cli_libelle) +
                        scale_y_continuous(trans = "reverse",
                                           expand = expansion(mult = c(0.05, 0.01))))
  
  return(graphique)
  
}

classesi2m2$cli_borne_inf)<- as.numeric(gsub(",", ".", classesi2m2$cli_borne_inf))

ploti2m2 <- ggplot(m_i2m2, aes(x=annee,y=moy))  +
  geom_point(colour='blue')+geom_line(aes(x = annee, y =moy),colour='blue')+
  ggtitle("Moyenne I2M2")+coord_cartesian(ylim=c(0,1))
ploti2m2<-gg_ajouter_arriere_plan_int(ploti2m2,classesi2m2)
print(ploti2m2)

plotaspt <- ggplot(m_aspt)+ aes(x=annee,y=moy)  +
  geom_point(colour='green')+geom_line(aes(x = annee, y =moy),colour='green')+
  ggtitle("Moyenne ASPT")+coord_cartesian(ylim=c(0,1))
plotpolyv <- ggplot(m_polyv)+ aes(x=annee,y=moy)  +
  geom_point(colour='orange')+geom_line(aes(x = annee, y =moy),colour='orange')+
  ggtitle("Moyenne polyvoltinisme")+coord_cartesian(ylim=c(0,1))
plotovov <- ggplot(m_ovov)+ aes(x=annee,y=moy)  +
  geom_point(colour='grey')+geom_line(aes(x = annee, y =moy),colour='grey')+
  ggtitle("Moyenne ovoviparité")+coord_cartesian(ylim=c(0,1))
plotshannon <- ggplot(m_shannon)+ aes(x=annee,y=moy)  +
  geom_point(colour='red')+geom_line(aes(x = annee, y =moy),colour='red')+
  ggtitle("Moyenne Indice de Shannon")+coord_cartesian(ylim=c(0,1))
plotrichesse <- ggplot(m_rich)+ aes(x=annee,y=moy)  +
  geom_point(colour='pink')+geom_line(aes(x = annee, y =moy),colour='pink')+
  ggtitle("Moyenne Richesse taxonomique")+coord_cartesian(ylim=c(0,1))


plot_grid(ploti2m2,plotaspt,plotpolyv,plotovov,plotshannon, plotrichesse)


#################################################################################
#                       IBD                                          #
#################################################################################

#----Exploration statistique : variance----

## Calcul de la variance par année pour chaque indice
var_par_annee_ibd <- clean_ibd %>%
  group_by(code_indice, annee) %>%
  summarise(var = var(resultat_indice, na.rm = TRUE), .groups = "drop")

# Graphique de la variance par année avec facet_wrap()
plot_var_annee_ibd <- ggplot(var_par_annee_ibd, aes(x = annee, y = var, color = code_indice, group = code_indice)) +
  geom_point() +
  geom_line() +
  facet_wrap(~code_indice, scales = "free_y") +
  theme_minimal() 
labs(title = "Variance des indices au fil des années", x = "Année", y = "Variance")

print(plot_var_annee_ibd)

### Calcul de la variance par station pour chaque indice
var_par_station_ibd <- clean_ibd %>%
  group_by(code_indice, code_station_hydrobio) %>%
  summarise(var = var(resultat_indice, na.rm = TRUE), .groups = "drop")

# Graphique de la variance par station avec facet_wrap()
plot_var_station_ibd <- ggplot(var_par_station_ibd, aes(x = code_station_hydrobio, y = var, fill = code_indice)) +
  geom_col(position = "dodge") +
  facet_wrap(~code_indice, scales = "free_y") +
  theme_minimal() 

theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Variance des indices par station", x = "Station", y = "Variance")

print(plot_var_station_ibd)


#----Exploration stat : médiane----

### Calcul de la médiane par année et par station

# Médiane interannuelle
med_par_annee_ibd <- clean_ibd %>%
  group_by(code_indice, annee) %>%
  summarise(med = median(resultat_indice, na.rm = TRUE),
            var=var(resultat_indice, na.rm = TRUE),
            ecart_type=sd(resultat_indice, na.rm = TRUE),
            .groups = "drop")

# Graphique 
ggplot(med_par_annee_ibd, aes(x = annee, y = med, color = as.factor(code_indice))) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin= med - ecart_type, ymax= med + ecart_type),width=0.2) +
  facet_wrap(~code_indice, scales = "free_y") +
  labs(title = "Médiane des indices par année",
       x = "Année",
       y = "Médiane du paramètre",
       color = "Indice") +
  theme_minimal()

# Médiane par station
med_par_station_ibd <- clean_ibd %>%
  group_by(code_indice, code_station_hydrobio) %>%
  summarise(med = median(resultat_indice, na.rm = TRUE), .groups = "drop")

# Graphique 
ggplot(med_par_station_ibd, aes(x = code_station_hydrobio, y = med, color = as.factor(code_indice))) +
  geom_col() +
  geom_line() +
  facet_wrap(~code_indice, scales = "free_y") +
  labs(title = "Médiane des indices par station",
       x = "Station",
       y = "Médiane de l'indice",
       color = "Indice") +
  theme_minimal() 
theme(axis.text.x = element_text(angle = 90, hjust = 1))


#----Exploration stat : quantiles----
### Calcul des quantiles par année et par station
quantiles_par_annee_ibd <- clean_ibd %>%
  group_by(code_indice, annee) %>%
  summarise(
    Q1 = quantile(resultat_indice, probs = 0.25, na.rm = TRUE),
    Q2 = quantile(resultat_indice, probs = 0.50, na.rm = TRUE),
    Q3 = quantile(resultat_indice, probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(quantiles_par_annee_ibd, aes(x = annee, color = as.factor(code_indice))) +
  geom_line(aes(y = Q1), linetype = "dashed") +
  geom_line(aes(y = Q2)) +
  geom_line(aes(y = Q3), linetype = "dotted") +
  facet_wrap(~code_indice, scales = "free_y") +
  labs(title = "Quantiles des indices par année",
       x = "Année",
       y = "Valeur de l'indice",
       color = "Indice") +
  theme_minimal()


#################################################################################
#                       Physico                                                 #
#################################################################################

#----Exploration statistique : variance----

## Calcul de la variance par année pour chaque indice
var_par_annee_pc <- parametres_physico %>%
  group_by(code_parametre, annee) %>%
  summarise(var = var(resultat, na.rm = TRUE), .groups = "drop")

# Graphique de la variance par année avec facet_wrap()
plot_var_annee_pc <- ggplot(var_par_annee_pc, aes(x = annee, y = var, color = code_parametre, group = code_parametre)) +
  geom_point() +
  geom_line() +
  facet_wrap(~code_parametre, scales = "free_y") +
  theme_minimal() 
labs(title = "Variance des indices au fil des années", x = "Année", y = "Variance")


### Calcul de la variance par station pour chaque indice
var_par_station_pc <- parametres_physico %>%
  group_by(code_parametre, code_station_hydrobio, libelle_station) %>%
  summarise(var = var(resultat, na.rm = TRUE), .groups = "drop")

# Graphique de la variance par station avec facet_wrap()
plot_var_station_pc <- ggplot(var_par_station_pc, aes(x = code_station_hydrobio, y = var, fill = code_parametre)) +
  geom_col(position = "dodge") +
  facet_wrap(~code_parametre, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Variance des indices par station", x = "Station", y = "Variance")
    
#----Exploration stat : médiane----

### Calcul de la médiane par année et par station

med_par_annee_pc <- parametres_physico %>%
  group_by(code_parametre, annee) %>%
  summarise(med = median(resultat, na.rm = TRUE),
            var=var(resultat, na.rm = TRUE),
            ecart_type=sd(resultat, na.rm = TRUE),
            .groups = "drop")

ggplot(med_par_annee_pc, aes(x = annee, y = med, color = as.factor(code_parametre))) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin= med - ecart_type, ymax= med + ecart_type),width=0.2) +
  facet_wrap(~code_parametre, scales = "free_y") +
  labs(title = "Médiane des indices par année",
       x = "Année",
       y = "Médiane du paramètre",
       color = "Indice") +
  theme_minimal()

# Calcul des médiannes par stations
med_par_station_pc <- parametres_physico %>%
  group_by(code_parametre, code_station_hydrobio) %>%
  summarise(med = median(resultat, na.rm = TRUE), .groups = "drop")
ggplot(med_par_station_pc, aes(x = code_station_hydrobio, y = med, color = as.factor(code_parametre))) +
  geom_col() +
  geom_line() +
  facet_wrap(~code_parametre, scales = "free_y") +
  labs(title = "Médiane des indices par station",
       x = "Station",
       y = "Médiane de l'indice",
       color = "Indice") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#----Exploration stat : quantiles----
### Calcul des quantiles par année et par station
quantiles_par_annee_pc <- parametres_physico %>%
  group_by(code_parametre, annee) %>%
  summarise(
    Q1 = quantile(resultat, probs = 0.25, na.rm = TRUE),
    Q2 = quantile(resultat, probs = 0.50, na.rm = TRUE),
    Q3 = quantile(resultat, probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(quantiles_par_annee_pc, aes(x = annee, color = as.factor(code_parametre))) +
  geom_line(aes(y = Q1), linetype = "dashed") +
  geom_line(aes(y = Q2)) +
  geom_line(aes(y = Q3), linetype = "dotted") +
  facet_wrap(~code_parametre, scales = "free_y") +
  labs(title = "Quantiles des indices par année",
       x = "Année",
       y = "Valeur de l'indice",
       color = "Indice") +
  theme_minimal()

#################################################################################
#                       IBD                                          #
#################################################################################

#----Exploration statistique : variance----

## Calcul de la variance par année pour chaque indice
var_par_annee_ibd <- clean_ibd %>%
  group_by(code_indice, annee) %>%
  summarise(var = var(resultat_indice, na.rm = TRUE), .groups = "drop")

# Graphique de la variance par année avec facet_wrap()
plot_var_annee_ibd <- ggplot(var_par_annee_ibd, aes(x = annee, y = var, color = code_indice, group = code_indice)) +
  geom_point() +
  geom_line() +
  facet_wrap(~code_indice, scales = "free_y") +
  theme_minimal() 
labs(title = "Variance des indices au fil des années", x = "Année", y = "Variance")

print(plot_var_annee_ibd)

### Calcul de la variance par station pour chaque indice
var_par_station_ibd <- clean_ibd %>%
  group_by(code_indice, code_station_hydrobio) %>%
  summarise(var = var(resultat_indice, na.rm = TRUE), .groups = "drop")

# Graphique de la variance par station avec facet_wrap()
plot_var_station_ibd <- ggplot(var_par_station_ibd, aes(x = code_station_hydrobio, y = var, fill = code_indice)) +
  geom_col(position = "dodge") +
  facet_wrap(~code_indice, scales = "free_y") +
  theme_minimal() 

theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Variance des indices par station", x = "Station", y = "Variance")

print(plot_var_station_ibd)


#----Exploration stat : médiane----

### Calcul de la médiane par année et par station

# Médiane interannuelle
med_par_annee_ibd <- clean_ibd %>%
  group_by(code_indice, annee) %>%
  summarise(med = median(resultat_indice, na.rm = TRUE),
            var=var(resultat_indice, na.rm = TRUE),
            ecart_type=sd(resultat_indice, na.rm = TRUE),
            .groups = "drop")

# Graphique 
ggplot(med_par_annee_ibd, aes(x = annee, y = med, color = as.factor(code_indice))) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin= med - ecart_type, ymax= med + ecart_type),width=0.2) +
  facet_wrap(~code_indice, scales = "free_y") +
  labs(title = "Médiane des indices par année",
       x = "Année",
       y = "Médiane du paramètre",
       color = "Indice") +
  theme_minimal()

# Médiane par station
med_par_station_ibd <- clean_ibd %>%
  group_by(code_indice, code_station_hydrobio) %>%
  summarise(med = median(resultat_indice, na.rm = TRUE), .groups = "drop")

# Graphique 
ggplot(med_par_station_ibd, aes(x = code_station_hydrobio, y = med, color = as.factor(code_indice))) +
  geom_col() +
  geom_line() +
  facet_wrap(~code_indice, scales = "free_y") +
  labs(title = "Médiane des indices par station",
       x = "Station",
       y = "Médiane de l'indice",
       color = "Indice") +
  theme_minimal() 
theme(axis.text.x = element_text(angle = 90, hjust = 1))


#----Exploration stat : quantiles----
### Calcul des quantiles par année et par station
quantiles_par_annee_ibd <- clean_ibd %>%
  group_by(code_indice, annee) %>%
  summarise(
    Q1 = quantile(resultat_indice, probs = 0.25, na.rm = TRUE),
    Q2 = quantile(resultat_indice, probs = 0.50, na.rm = TRUE),
    Q3 = quantile(resultat_indice, probs = 0.75, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(quantiles_par_annee_ibd, aes(x = annee, color = as.factor(code_indice))) +
  geom_line(aes(y = Q1), linetype = "dashed") +
  geom_line(aes(y = Q2)) +
  geom_line(aes(y = Q3), linetype = "dotted") +
  facet_wrap(~code_indice, scales = "free_y") +
  labs(title = "Quantiles des indices par année",
       x = "Année",
       y = "Valeur de l'indice",
       color = "Indice") +
  theme_minimal()



save(med_par_station_pc,
     file = "Data/40_statistiques_descriptives.rda"
  
)
