recuperer_donnees_physico <- function(liste_stations,liste_parametres){
  library(purrr)
  library(hubeau)
  library(dplyr)
  
  ##creation d'une fonction unitaire à 2 paramètres
  f_get_physico_station <- function(station)
  {
    
    return(get_qualite_rivieres_analyse_pc(code_station=station,code_parametre=c("1295","1301","1302","1303","1311","1312","1313","1305","1309","1350",
                                                                                 "1340", "1339", "1433", "1335", "1841"),
                                           date_debut_prelevement='2015-01-01'))
  }
  return(map_df(liste_stations,f_get_physico_station))
}


#exemple DATA <- recuperer_donnees_physico(c('04182000','04182510'))
# attention les codes stations et paramètres entre guillemets
#f_get_physico_station("04212100")