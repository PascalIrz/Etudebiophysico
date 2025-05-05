parametres_physico %>% View


mean_physicochimie<- parametres_physico %>%
  filter(format(as.Date(date_prelevement), "%m")%in% c("04","05","06","07","08","09", "10")) %>%
  group_by(code_station_hydrobio,annee,code_parametre,libelle_parametre) %>%
  summarise(moyenne_resultat=mean(resultat, na.rm=TRUE))

mean_trans<- mean_physicochimie %>% 
  select(code_station_hydrobio,annee,code_parametre,moyenne_resultat) %>% 
  distinct() %>% 
  pivot_wider(names_from = "code_parametre",
              values_from = "moyenne_resultat") %>% 
  select(`1295`:`1350`)




