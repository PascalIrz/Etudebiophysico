# exploration des métriques de l'I2M2 à partir de Hub'eau

#----Mise en place du jeu de données----

#----Chargement des librairies----
library(hubeau)
library(tidyverse)
library(sf)
library(cowplot)
library(DT)
library(httr)#utiliser POST pour calcul i2m2 à partir du SEEE
library(trend)
library(ggrepel)
library(mapview)
library(lubridate)

functions <- list.files(path = "R",
                        pattern = ".R$",
                        full.names = TRUE)

map(.x = functions,
    .f = source)

# source(file="R/Fonctions_unitaires_appel_API.R")
# source(file = "R/mk_st_by_group.R")
# source(file = "R/Mann_kendall_div.R")
# source(file = "../Exploitationindicesminv/R/Seee_to_df.R")
# source(file = "../Exploitationindicesminv/R/Calcule_I2M2_métriques.R")
# source(file ="Script/Seee_diat_to_dataframe.R")
# source(file ="Script/calcule_SEEE_IBD.R")

#################################################################################
#                       import des métriques I2M2                               #
#################################################################################

dep <- c('22','29','35','56') # choix des numéros de département

## I2M2 et métriques
##l'interrogation de l'API est couteuse
# aussi , on le fait une fois et on enregistre les donnees dans le fichier Multi_indice_minv
if (file.exists("Data/Multi_indice_minv_all.Rdata"))
{
  load("Data/Multi_indice_minv_all.Rdata")
} else
{
  Multi_indice_minv <- map_df(dep,f_get_minv_departement)%>%
    mutate(annee=year(date_prelevement)) %>%
    select("code_station_hydrobio","code_qualification","libelle_qualification","libelle_indice","libelle_station_hydrobio","date_prelevement","code_indice","resultat_indice","latitude","longitude","code_departement","annee") %>% 
    arrange(code_station_hydrobio,annee)
  
  save(Multi_indice_minv,file="Data/Multi_indice_minv_all.Rdata")
}

## rajout des résultats de l'année (non qualifiés)
#on checke avant si on a de la données dans Fichier_SEEE/
Liste_fichiers <-  list.files("../Fichiers_SEEE//",pattern="*.txt")
if (length(Liste_fichiers!=0)){
  data_entree <- Seee_to_df("../Fichiers_SEEE/")
  donnees_metriques_complementaires <- calcule_SEEE_I2M2(data_entree) %>% 
    rename(code_station_hydrobio=CODE_STATION) %>%
    rename(code_indice=CODE_PAR) %>% 
    rename(resultat_indice=RESULTAT) %>% 
    filter(!is.na(resultat_indice)) %>% select(2,3,4,6) %>% 
    rename(date_prelevement=DATE) %>% 
    mutate(annee=str_sub(date_prelevement,start=7,end=10))#le sul moyen que j'ai trouvé pour recupérer l'année !
  Stations <- select(Multi_indice_minv,code_station_hydrobio,libelle_station_hydrobio,latitude,longitude,code_departement)%>%
    unique()
  
  donnees_metriques_complementaires <- left_join(donnees_metriques_complementaires,Stations,by='code_station_hydrobio') %>% 
    select(1,6,2,3,4,7,8,9,5)
  #on amende le DF initial
  Multi_indice_minv <- rbind(Multi_indice_minv,donnees_metriques_complementaires) %>%   arrange(code_station_hydrobio,annee)
}else {
  print ("pas de données complémentaires trouvées dans le dossier Data")
}

##

## Stations non retenues (donnéees < 5 ans)

#on vire s'il y a peu de données par paramètre (au moins 4 donnees/station/paramètre)
#on suppose que pour chaque indice/metrique on a le même nb d'analyses
#on va donc compter pour i2m2 et virer les stations "pauvres" en données
comptemulti <-
  count(Multi_indice_minv, code_station_hydrobio, code_indice) %>%
  filter(n > 5) %>% filter(code_indice == 7613) %>%   select("code_station_hydrobio")

Non_retenu <-
  count(Multi_indice_minv,
        code_station_hydrobio,
        code_indice,
        libelle_station_hydrobio) %>%  filter(n <= 3) %>% filter(code_indice == 7613) %>% select("code_station_hydrobio", "libelle_station_hydrobio", "n") %>% rename("Nb prélèvements" =
                                                                                                                                                                        n)
datatable(
  Non_retenu,
  class = 'cell-border stripe',
  options =
    list(
      iDisplayLength = 10,
      bLengthChange = TRUE,
      bFilter = TRUE ,
      bInfo = TRUE,
      rowid = FALSE,
      autoWidth = FALSE,
      ordering = TRUE,
      scrollX = TRUE,
      borders = TRUE,
      columnDefs = list(list(
        className = 'dt-center', targets = "_all"
      ))
    ),
  rownames = FALSE#enlever le numero des lignes
)


#Stations_a_garder <- unique(comptemulti$code_station_hydrobio)
Multi_indice_minv_s <-
  filter(
    Multi_indice_minv,
    Multi_indice_minv$code_station_hydrobio %in% comptemulti$code_station_hydrobio
  ) %>% arrange(code_station_hydrobio, annee) #normalement la tri est deja fait plus haut mais je me méfie !

#----Obtention du jeu de données sur lequel on va travailler

#On nettoie le jeu de donnée on ne garde seulement que les indices qui nous intéresse
clean_minv <- Multi_indice_minv_s %>% 
  filter(code_indice  %in% c('8058', '8054', '8056', '8055', '8057', '7613',
         libelle_qualification != "incertaine") #on enlève les valeurs incertaines
  )


#On enlève les doublons
clean_minv <- clean_minv %>%
  distinct()

#On compte les prelevement par an et par stations
compte_prelevement_i2m2 <- clean_minv %>%
  group_by(code_station_hydrobio, annee) %>%
  summarise(n_prelevement = n(), .groups = "drop")

compte_prelevement_i2m2 %>%
  filter(n_prelevement > 6)

mois_repartition <- clean_minv %>%
  mutate(mois = month(as.Date(date_prelevement))) %>%  # transforme en date si besoin
  count(mois, name = "nombre_prelevements") %>%        # compte le nombre d'observations par mois
  arrange(desc(nombre_prelevements))                   # trie pour voir le mois le plus fréquent en premier


# Afficher le résultat
print(mois_repartition)

clean_minv <- clean_minv %>%
  filter(!(code_station_hydrobio == "04175500" & date_prelevement == as.Date("2022-08-08T00:00:00Z")),
         !(code_station_hydrobio == "04177050" & date_prelevement == as.Date("2023-09-26T00:00:00Z")),
         !(code_station_hydrobio == "04216050" & date_prelevement == as.Date("2020-10-13T00:00:00Z")))

#On visualise les stations 
stations_map<- clean_minv %>%
  select(code_station_hydrobio,libelle_station_hydrobio,longitude,latitude) %>%
  distinct()

mapview(
  stations_map,
  xcol = "longitude",
  ycol = "latitude",
  crs = 4326,
  grid = FALSE,
  layer.name = "Stations",
  cex = 4
) #stations I2M2

mapview(
  stations_map %>% filter(code_station_hydrobio == "04195000"),
  xcol = "longitude",
  ycol = "latitude",
  crs = 4326,
  grid = FALSE,
  layer.name = "Stations",
  cex = 4
)


#################################################################################
#                       import IBD                                              #
#################################################################################
if (file.exists("Data/Indice_ibd.Rdata")) {
  load("Data/Indice_ibd.Rdata")
} else {
  Indice_ibd <- map_df(dep,f_get_ibd_departement)%>%
    mutate(date_prelevement = as.Date(sub("T.*", "", date_prelevement))) %>% 
    mutate(annee=year(date_prelevement)) %>%
    select(-c(coordonnee_x,coordonnee_y,uri_station_hydrobio,code_projection,code_cours_eau,libelle_cours_eau,uri_cours_eau,code_masse_eau,libelle_masse_eau,uri_masse_eau,code_sous_bassin,libelle_sous_bassin,code_bassin,libelle_bassin,code_commune,libelle_commune, libelle_departement,code_region,libelle_region,code_support,libelle_support, code_methode,geometry.type,geometry.crs.type,geometry.crs.properties.name,geometry.coordinates1,
              geometry.coordinates2,code_prelevement,code_banque_reference,code_operation_prelevement)) %>% 
    filter(!libelle_qualification%in%c("incertaine","non définissable","non qualifié","incorrecte")) %>%
    filter(!libelle_indice%in%c("Nombre de taxons contributifs de l'IBD","Nombre d'unités diatomiques contributives","Nombre de taxons pris en compte pour le calcul de l'IBD ancien","Indice Biologique Diatomées ancien")) %>%
    arrange(code_station_hydrobio,annee)
} 

# On importe le jeu de données complémentaire
diatcomplement <- Seee_diat_to_dataframe("Data/donnees/")

indicesdiatcomplement <- calcule_SEEE_IBD(diatcomplement) %>% 
  rename(c(code_station_hydrobio = CODE_STATION, date_prelevement = DATE, libelle_indice = LIB_PAR, resultat_indice= RESULTAT, code_indice = CODE_PAR)) %>% 
  mutate(date_prelevement = if_else(
    grepl("/", date_prelevement),
    as.character(as.Date(date_prelevement, format = "%d/%m/%Y")),
    as.character(date_prelevement)),
date_prelevement = as.Date(date_prelevement),
annee = format(date_prelevement, "%Y")) %>% 
  filter(!libelle_indice%in%c("NbTaxonsIBDcontributifs","NbUniteDiatomique")) %>%
  filter(!code_station_hydrobio == "0J710540T") %>%
  mutate(libelle_indice = recode(libelle_indice,
                                 "IndiceBioDiat" = "Indice Biologique Diatomées (IBD)")) %>% 
  select(libelle_indice, code_station_hydrobio,date_prelevement, resultat_indice) 

Indice_ibd <- bind_rows(Indice_ibd, indicesdiatcomplement)

save(Indice_ibd,file="Data/Indice_ibd.Rdata")



# On garde seulement les stations où l'on a + de 5 années de données
comptemulti_ibd <-
  count(Indice_ibd, code_station_hydrobio, code_indice) %>% 
  filter(n > 5) %>% filter(code_indice == 5856) %>%
  select("code_station_hydrobio") 

# On transforme le jeu de données 
clean_ibd <-
  filter(
    Indice_ibd,
    Indice_ibd$code_station_hydrobio %in% comptemulti_ibd$code_station_hydrobio
  ) %>% arrange(code_station_hydrobio, annee)


nb_stations <- clean_ibd %>%
  distinct(code_station_hydrobio) %>%
  nrow()
print(nb_stations)

# On sait que les années 2005 et 2006 ont très peu de données, on les enleve 

clean_ibd <- clean_ibd %>% 
  filter(!(code_indice == "5856" & annee %in% c(2005, 2006, 2007)))

#Nombre total d'analyses par année 
nb_analyses_par_annee_ibd<- clean_ibd %>%
  filter(code_indice == "5856") %>% 
  group_by(annee)%>%
  summarise(nb_analyses=n(),.groups="drop")
nb_analyses_par_annee_ibd

#nb de prelevement par stations par an 
compte_prelevement_ibd <- clean_ibd %>% 
  group_by(code_station_hydrobio,annee) %>% 
  summarise(n_prelevement = n(), .groups="drop")
compte_prelevement_ibd %>% 
  filter(n_prelevement>2)

mois_repartition <- clean_ibd %>%
  mutate(mois = month(as.Date(date_prelevement))) %>%  # transforme en date si besoin
  count(mois, name = "nombre_prelevements") %>%        # compte le nombre d'observations par mois
  arrange(desc(nombre_prelevements))                   # trie pour voir le mois le plus fréquent en premier


# Afficher le résultat
print(mois_repartition)

#J'enlève les prélevements en trop : on garde les mois les plus représentés 

clean_ibd <- clean_ibd %>%
  filter(!(code_station_hydrobio == "04164870" & date_prelevement == as.Date("2008-09-30")),
         !(code_station_hydrobio == "04174480" & date_prelevement == as.Date("2010-09-16")),
         !(code_station_hydrobio == "04175500" & date_prelevement == as.Date("2022-09-29")),
         !(code_station_hydrobio == "04176000" & date_prelevement == as.Date("2008-08-13")),
         !(code_station_hydrobio == "04178110" & date_prelevement == as.Date("2014-07-03")),
         !(code_station_hydrobio == "04178110" & date_prelevement == as.Date("2015-07-28")),
         !(code_station_hydrobio == "04178127" & date_prelevement == as.Date("2008-08-12")),
         !(code_station_hydrobio == "04190842" & date_prelevement == as.Date("2008-10-29")),
         !(code_station_hydrobio == "04195190" & date_prelevement == as.Date("2020-10-13")),
         !(code_station_hydrobio == "04196000" & date_prelevement == as.Date("2008-09-18")),
         !(code_station_hydrobio == "04196000" & date_prelevement == as.Date("2020-07-21")),
         !(code_station_hydrobio == "04197818" & date_prelevement == as.Date("2013-06-24")),
         !(code_station_hydrobio == "04212100" & date_prelevement == as.Date("2011-06-24")),
         !(code_station_hydrobio == "04214500" & date_prelevement == as.Date("2010-07-05")),
         !(code_station_hydrobio == "04338000" & date_prelevement == as.Date("2018-09-04")))


#nombre d'années par station
nb_annees_par_station_ibd <- clean_ibd %>%
  group_by(code_station_hydrobio) %>%
  summarise(nb_annees = n_distinct(annee), .groups = "drop")

# Affichage du résultat
print(nb_annees_par_station_ibd)

#On enlève les doublons
clean_ibd <- clean_ibd %>% 
  distinct()
  
#on vérifie s'il y a des doublons
count(clean_ibd,code_station_hydrobio,code_indice,date_prelevement,resultat_indice) %>% filter(n>1 & code_indice == "5856")

# On transpose les valeurs pour créer par la suite le df global qui va servir aux matrices de corrélations 
ibd_trans <- clean_ibd %>%
  select(code_station_hydrobio,annee,code_indice,resultat_indice) %>% 
  distinct() %>% 
  pivot_wider(names_from = "code_indice",
              values_from = "resultat_indice")



#################################################################################
#                       import physico-chimie                                   #
#################################################################################
# On rassemble l'ensemble des codes stations qu'elles aient de la donnée diat ou inv
stations_inv <- unique(clean_minv$code_station_hydrobio)
stations_ibd <- unique(clean_ibd$code_station_hydrobio)

stations <- c(stations_inv, stations_ibd) %>% 
  unique()

# Chargement seulement s'il n'y a pas déjà un fichier de données dans le sous-répertoire Data
if (file.exists("Data/parametres_physico.Rdata")) {
  
  load("Data/parametres_physico.Rdata")  # Charger les données existantes
  
} else {
  
  parametres_physico <- recuperer_donnees_physico(stations)  # Récupérer les données
  save(parametres_physico, file = "Data/parametres_physico.Rdata")  # Sauvegarder
  
}


# on avait 233 stations avec de la bio et seulement 232 avec de la physico => pb ?
setdiff(nb_annees_par_station_ibd$code_station_hydrobio,
        unique(parametres_physico$code_station))

##On nettoie jeu de donnee

parametres_physico <- parametres_physico %>%
  rename(code_station_hydrobio = code_station)

  
parametres_physico <- parametres_physico %>% 
  dplyr::select(code_station_hydrobio,
    libelle_station,
    libelle_fraction,
    code_fraction,
    code_parametre,
    resultat,
    libelle_parametre,
    date_prelevement,
    code_support
  ) %>%
  filter(code_support == 3) # seulement l'eau (on enlève les sédiments)

parametres_physico <- parametres_physico %>%
  filter(!is.na(resultat)) %>%
  mutate(annee = year(date_prelevement)) %>%
  arrange(code_station_hydrobio, annee) %>%
  mutate(mois = month(date_prelevement)) %>%
  mutate(jour = day(date_prelevement))

parametres_physico <- parametres_physico %>% 
  mutate(
    resultat_arcsin = case_when(
      code_parametre %in% c("1335","1339") ~ asin(sqrt(resultat)),
      TRUE ~ NA_real_
    )
           )


parametres_physico <- parametres_physico %>%
  mutate(
    resultat = case_when(
      libelle_parametre == "Potentiel en Hydrogène (pH)" & code_station_hydrobio == "04167700" & date_prelevement == "2024-12-11" ~ 7.9,
      TRUE ~ resultat
    )
  )


# On ne garde que l'eau filtrée pour les nitrates, nitrites, ammonium, orthophosphates et carbone organique 
parametres_physico <- parametres_physico %>% 
    filter(!(code_parametre %in% c("1340","1339","1335","1433","1841") & 
                                     code_fraction %in% c(22,23)))

# On ne garde seulement l'eau brute pour les paramètres physiques, MES et P tot
parametres_physico <- parametres_physico %>% 
  filter(!(code_parametre %in% c("1311","1312","1295","1302","1350","1301","1313","1303","1305") & 
             code_fraction %in% c(22,3)))

#On enlève les doublons 
parametres_physico <- unique(parametres_physico)

# On enlève les valeurs aberrantes
#parametres_physico <- parametres_physico %>% 
  #group_by(code_parametre, annee) %>%
  #mutate(
    #moyenne = mean(resultat, na.rm = TRUE),
    #ecart_type = sd(resultat, na.rm = TRUE),
    #seuil_min = moyenne - 4 * ecart_type,
    #seuil_max = moyenne + 4 * ecart_type
  #) %>%
  #ungroup() %>%
  #filter(resultat >= seuil_min & resultat <= seuil_max)
  #select(-moyenne, -ecart_type, -seuil_min, -seuil_max)

#On essaye une autre méthode :
comparaison_outliers <- parametres_physico %>%
  group_by(code_parametre) %>%
  mutate(
    # Méthode IQR
    Q1 = quantile(resultat, 0.25, na.rm = TRUE),
    Q3 = quantile(resultat, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_iqr = Q1 - 1.5 * IQR,
    upper_iqr = Q3 + 1.5 * IQR,
    outlier_iqr = resultat < lower_iqr | resultat > upper_iqr,
    
    # Méthode des percentiles extrêmes (1% et 99%)
    lower_pct = quantile(resultat, 0.01, na.rm = TRUE),
    upper_pct = quantile(resultat, 0.99, na.rm = TRUE),
    outlier_pct = resultat < lower_pct | resultat > upper_pct
  )

# Comparaison des résultats
comparaison_table <- comparaison_outliers %>%
  summarise(
    total = n(),
    outliers_iqr = sum(outlier_iqr, na.rm = TRUE),
    outliers_pct = sum(outlier_pct, na.rm = TRUE),
    concordance = sum(outlier_iqr == outlier_pct, na.rm = TRUE)
  )

# Affichage des résultats
print(comparaison_table)

#On choisie d'utiliser IQR 
parametres_physico <- parametres_physico %>%
  group_by(code_parametre) %>%
  mutate(Q1 = quantile(resultat, 0.25, na.rm = TRUE),
         Q3 = quantile(resultat, 0.75, na.rm = TRUE),
         IQR = Q3 - Q1,
         lower = Q1 - 1.5 * IQR,
         upper = Q3 + 1.5 * IQR)
  #filter(resultat >= lower & resultat <= upper) %>%
  #select(-Q1, -Q3, -IQR, -lower, -upper)  # Supprime les colonnes temporaires


#On renvoie les valeurs supprimées 
valeurs_supp <- parametres_physico %>% 
  filter(resultat < lower | resultat > upper)

#On filtre parametre_physico
parametres_physico <- parametres_physico %>% 
  filter(resultat >= lower & resultat <= upper) %>% 
  dplyr::select(-Q1, -Q3, -IQR, -lower, -upper)


# Df utiles pour la fenêtre glissante
stations_parametre <-
  unique(parametres_physico$code_station_hydrobio)
code_pc <- unique(parametres_physico$code_parametre)

#Nombre total d'analyses par année
nb_analyses_par_annee_pc <- parametres_physico %>%
  group_by(annee) %>%
  summarise(nb_analyses = n(), .groups = "drop")
print(nb_analyses_par_annee_pc)

#nombre d'années par station
nb_annees_par_station_pc <- parametres_physico %>%
  group_by(code_station_hydrobio) %>%
  summarise(nb_annees = n_distinct(annee), .groups = "drop")

# Affichage du résultat
print(nb_annees_par_station_pc)


#on regarde s'il y a des doublons
count(
  parametres_physico,
  code_station_hydrobio,
  code_parametre,
  mois,
  annee,
  jour,
  resultat
) %>% filter(n > 1 & code_parametre %in% code_pc)

parametre_trans <- parametres_physico %>%
  dplyr::select(code_station_hydrobio,
                annee,
                mois,
                jour,
                code_parametre,
                resultat) %>%
  distinct() %>%
  pivot_wider(names_from = "code_parametre",
              values_from = "resultat",
              values_fn = mean)

#################################################################################
#                       Sauvegarde                                              #
#################################################################################

save(clean_ibd,
     clean_minv,
     parametres_physico,
     parametre_trans,
     ibd_trans,
     code_pc,
     stations_parametre,
     file = "Data/10_donnees_pretraitees.rda")
