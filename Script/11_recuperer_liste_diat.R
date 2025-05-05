library(hubeau)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)
library(dplyr)
library(purrr)

recuperer_liste_diat <- function(){
#la requete est trop lourde, on ne peut pas faire comme précédement par département, on va faire par station

#la requete est faite pour le RCS, on ne la refait pas
if (file.exists("data/Liste_taxo_diat.Rdata")){
  load("data/Liste_taxo_diat.Rdata") } else
  {
    f_get_liste_taxo_diat <- function(cdstation){
      #print(cdstation)
      get_hydrobio_taxons(code_station_hydrobio=cdstation,code_support=10)#10 pour diat
    }
    print("récupération des données à partir d'Hubeau")
    Liste_taxo_diat <- map_df(Stations,f_get_liste_taxo_diat) %>%
      #attention on n'a pris qu'une station, il faudra ensuite choisir Stations[,1]
      select(1:8) %>%
      mutate(annee=year(date_prelevement))
  }
return(Liste_taxo_diat)
}

# exemple de traitement
Stations <- '04199370'# pour exemple; normalement Stations est une liste de codes station
Liste_taxo_diat <- recuperer_liste_diat()
#exploration des données
Liste_prel <- select(Liste_taxo_diat,code_station_hydrobio,libelle_station_hydrobio,date_prelevement,annee)
nbtaxonsparstation <- group_by(Liste_prel,code_station_hydrobio,date_prelevement,annee) %>% mutate(nb=n()) %>% unique()

# nb de prel par annee/station 
# 
Liste_prel <- 
  unique(Liste_prel) %>% 
  group_by(code_station_hydrobio, annee) %>% 
  mutate(nbprel=n()) %>% select(code_station_hydrobio,annee,nbprel) %>% 
  unique()



