#Saisie_minv_to_seee
#' Title
#'
#' @Data
#'
#' @return
#' @export
#'
#' @examples
#extrait un dataframe de tous les fichiers de saisie
Seee_diat_to_dataframe<- function(chemin_data){

  premiereligne <- c("CODE_OPERATION","CODE_STATION",	"LB_STATION",
                         "CODE_POINT",	"DATE",	"CODE_PRODUCTEUR",
                         "NOM_PRODUCTEUR",	"CODE_PRELEVEUR",
                         "NOM_PRELEVEUR",	"CODE_DETERMINATEUR",
                         "NOM_DETERMINATEUR",	"COORD_X_OP",	"COORD_Y_OP",
                         "COORD_X_OP_AVAL",	"COORD_Y_OP_AVAL",
                         "TYPO_NATIONALE",	"CODE_PHASE",	"CODE_PREL_ELEM",
                         "CODE_TAXON",	"NOM_LATIN_TAXON",
                         "RESULTAT",	"CODE_REMARQUE")

  chemin_data <- paste("C:/Users/ilona.garcia/Documents/RstudioGIT/Exploitationindicesminv/Data/donnees","/",sep="")

  Df_entree_seee <- data.frame()

  Liste_fichiers <-
    list.files(chemin_data,pattern=".txt",full.names = T )
  for (fichier_unitaire in Liste_fichiers){
    print(paste("Fichier en cours: ", fichier_unitaire))
    Fichier <- paste(chemin_data,fichier_unitaire,sep="")
    data <- read.table(fichier_unitaire, header = TRUE,sep='\t',
                       colClasses = c(CODE_OPERATION = "character",
                                      CODE_STATION   = "character",
                                      CODE_TAXON     = "character")) %>%
      filter(RESULTAT > 0) %>%
      as.data.frame()
    Df_entree_seee <- rbind(data,Df_entree_seee)

   }




  #colnames(Df_entree_seee) <- premiereligne
  return(Df_entree_seee)


}




