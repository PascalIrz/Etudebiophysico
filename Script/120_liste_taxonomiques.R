library(hubeau)
# library(dplyr)
# library(stringr) #pour str_extract
# library(ggplot2)
library(tidyverse)
library(sf)
# library(purrr)
library(cowplot)
library(DT)
library(httr)#utiliser POST pour calcul i2m2 à partir du SEEE
library(trend)
library(ggrepel)
library(mapview)
library(lubridate)
library(httr)

functions <- list.files(path = "R",
                        pattern = ".R$",
                        full.names = TRUE)

map(.x = functions,
    .f = source)


if (file.exists("Data/ListeTaxonomique.Rdata"))
{
  load("Data/ListeTaxonomique.Rdata")
#} else
#{
  #liste_taxo <- map_df(dep,f_get_liste_taxo_minv)%>%
    
  #save(liste_taxo,file="Data/ListeTaxonomique.Rdata")
}


#on fait le même tri : sup à 5 années de suivi
# on est sensé avoir 112 stations de 2015 à 2023
#compter le nombre de taxon différents par stations au cours du temps
#les identifier
#calculer les indices EPT? GOLD? 
#tableau de contingence : AFC selon taille du BV ? Selon station seulement ? 
#Traits écologiques 

