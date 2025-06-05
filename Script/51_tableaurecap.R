load(file = "Data/10_donnees_pretraitees.rda")
load(file = "Data/50_chroniques_et_tendance.rda")
source(file = "R/mk_st_by_group.R")
source(file = "R/Mann_kendall_div.R")

# On charge les fonctions utiles
functions <- list.files(path = "R",
                        pattern = ".R$",
                        full.names = TRUE)

map(.x = functions,
    .f = source)

#Tableau avec les stations, sens de la pente, valeur de la pente, rapide/lent




