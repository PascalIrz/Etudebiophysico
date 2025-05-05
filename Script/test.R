prov <- parametres_physico %>% 
  select(code_station_hydrobio,
         code_parametre,
         annee,
         mois,
         resultat) %>% 
  distinct() %>% 
  group_by(code_station_hydrobio,
           code_parametre,
           annee,
           mois) %>% 
  tally()
  