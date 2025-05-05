donnees_i2m2<- function(dep, seee_folder) { 
  source(file="R/Fonctions_unitaires_appel_API.R")
  
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
      select("code_station_hydrobio","libelle_indice","libelle_station_hydrobio","date_prelevement","code_indice","resultat_indice","latitude","longitude","code_departement","annee") %>% 
      arrange(code_station_hydrobio,annee)
    
    save(Multi_indice_minv,file="Data/Multi_indice_minv_all.Rdata")
  }
  
  # ## rajout des résultats de l'année (non qualifiés)
  # #on checke avant si on a de la données dans Fichier_SEEE/
  # Liste_fichiers <-  list.files("../Fichiers_SEEE//",pattern="*.txt")
  # if (length(Liste_fichiers!=0)){
  #   data_entree <- Seee_to_df("../Fichiers_SEEE/")
  #   donnees_metriques_complementaires <- calcule_SEEE_I2M2(data_entree) %>% 
  #     rename(code_station_hydrobio=CODE_STATION) %>%
  #     rename(code_indice=CODE_PAR) %>% 
  #     rename(resultat_indice=RESULTAT) %>% 
  #     filter(!is.na(resultat_indice)) %>% select(2,3,4,6) %>% 
  #     rename(date_prelevement=DATE) %>% 
  #     mutate(annee=str_sub(date_prelevement,start=7,end=10))#le sul moyen que j'ai trouvé pour recupérer l'année !
  #   Stations <- select(Multi_indice_minv,code_station_hydrobio,libelle_station_hydrobio,latitude,longitude,code_departement)%>%
  #     unique()
  #   
  #   donnees_metriques_complementaires <- left_join(donnees_metriques_complementaires,Stations,by='code_station_hydrobio') %>% 
  #     select(1,6,2,3,4,7,8,9,5)
  #   #on amende le DF initial
  #   Multi_indice_minv <- rbind(Multi_indice_minv,donnees_metriques_complementaires) %>%   arrange(code_station_hydrobio,annee)
  # }else {
  #   print ("pas de données complémentaires trouvées dans le dossier Data")
  # }
  
  ##
  
  ## Stations non retenues (donnéees < 4 ans)
  
  #on vire s'il y a peu de données par paramètre (au moins 4 donnees/station/paramètre)
  #on suppose que pour chaque indice/metrique on a le même nb d'analyses
  #on va donc compter pour i2m2 et virer les stations "pauvres" en données
  comptemulti <- count(Multi_indice_minv, code_station_hydrobio, code_indice) %>%  filter(n>3) %>% filter(code_indice==7613) %>%   select("code_station_hydrobio")
  Non_retenu <- count(Multi_indice_minv,code_station_hydrobio,code_indice,libelle_station_hydrobio) %>%  filter(n<=3) %>% filter(code_indice==7613) %>% select("code_station_hydrobio","libelle_station_hydrobio","n") %>% rename("Nb prélèvements"=n)
  datatable(Non_retenu,class = 'cell-border stripe',options =
              list( iDisplayLength=10,
                    bLengthChange=TRUE,                       
                    bFilter=TRUE ,                                   
                    bInfo=TRUE,
                    rowid = FALSE,
                    autoWidth = FALSE,
                    ordering = TRUE,
                    scrollX = TRUE,
                    borders = TRUE,
                    columnDefs = list(list(className = 'dt-center', targets ="_all"))
              ),rownames=FALSE#enlever le numero des lignes
  )
  
  
  #Stations_a_garder <- unique(comptemulti$code_station_hydrobio)
  Multi_indice_minv_s <- filter(Multi_indice_minv,Multi_indice_minv$code_station_hydrobio%in%comptemulti$code_station_hydrobio) %>% arrange(code_station_hydrobio,annee) #normalement la tri est deja fait plus haut mais je me méfie !
  
  #On nettoie le jeu de donnée on ne garde seulement que les indices qui nous intéresse
  clean_minv<-filter(Multi_indice_minv_s,code_indice  %in% c('8058','8054','8056','8055','8057','7613'))
  
  return(clean_minv)
}
