Classes_carayon <- function(Listediat){
  library(lubridate)# pour utiliser la fct year()
  library(dplyr)# a enlever si appelé par ailleurs
  library(hubeau)#a enlever si appelé par ailleurs

#entree : DF contenant les listes taxo :station, date, code taxon, abondance

# pour tester la fct on se cree ici un DF
  test <- f_get_liste_taxo_diat('04171050') %>% 
    rbind(f_get_liste_taxo_diat('04171010')) 
  test$code_appel_taxon <- as.numeric(test$code_appel_taxon)#pour jointure future
# fin test, a supprimer ensuite
#prevoir peut ertre un filtre pour :
  # enlever les taxons < 10
  #enlever les taxons si < 5%

Tablecorrespondance <- readxl::read_xlsx(('Data/Correspondancetaxdiat.xlsx'))
Carayontaxons <- readxl::read_xlsx(('data/carayonmodalites.xlsx')) %>% 
  left_join(Tablecorrespondance,by=c('code'='AFNOR'))
Listeavecmodalites <-  left_join(test,Carayontaxons,by=c("code_appel_taxon"="SANDRE")) %>% 
  filter(!is.na(Class))
################ calcul des classes par prel
Classescarayon <- Listeavecmodalites %>% 
  mutate(resmod=resultat_taxon*Class) %>% 
  group_by(code_station_hydrobio,date_prelevement,parameter) %>%
  summarise(resultat=sum(resmod/sum(resultat_taxon)))
  
return(Classescarayon)

}

