Classes_carayon <- function(Listediat){
  library(lubridate)# pour utiliser la fct year()
  library(dplyr)# a enlever si appelé par ailleurs
  library(hubeau)#a enlever si appelé par ailleurs
  library(tidyr)#pour utiliser pivot_wider

#entree : DF contenant les listes taxo :station, date, code taxon, abondance

############# test avec le DF d'Ilona (a supprimer)
  # load('/Data/df_abondance_rel_diat.RData')
  # Listediat <- df1_ligne1
  ############################################
  
  Listediat$code_appel_taxon <- as.numeric(Listediat$code_appel_taxon)#pour jointure future
  #calcul abondance totale
  # abondancetotale <- Listediat %>% select(1,3,6) %>% 
  #   group_by(code_station_hydrobio,date_prelevement) %>% 
  #   summarise(abondance_ttale=sum(resultat_taxon))
  # test2 <- left_join(test,abondancetotale,by="code_station_hydrobio") %>% 
  #   mutate(abondancerelative=resultat_taxon*100/abondance_ttale) %>% 
  #   filter(abondancerelative>5) %>% unique(code_station_hydrobio,date_prelevement,code_appel_taxon)
  # Listediat <- select(test2,1:7)
  
  
Tablecorrespondance <- readxl::read_xlsx(('Data/Correspondancetaxdiat.xlsx'))
Carayontaxons <- readxl::read_xlsx(('data/carayonmodalites.xlsx')) %>% 
  left_join(Tablecorrespondance,by=c('code'='AFNOR'))
Listeavecmodalites <-  left_join(Listediat,Carayontaxons,by=c("code_appel_taxon"="SANDRE"),
                                 relationship="many-to-many") %>% 
  filter(!is.na(Class))
################ calcul des classes par prel
Classescarayon <- Listeavecmodalites %>% 
  mutate(resmod=round(total_abondance*Class)) %>% 
  group_by(code_station_hydrobio,annee,parameter) %>%
  summarise(resultat=sum(resmod/sum(total_abondance))) %>% 
  pivot_wider(names_from = parameter,values_from = resultat)
  
return(Classescarayon)

}

