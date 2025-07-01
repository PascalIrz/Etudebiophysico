load("Data/liste_taxon_ajout.Rdata")
load("Data/df_taxo.rda")
source(file = "R/mk_st_by_group.R")
source(file = "R/Mann_kendall_div.R")

#Librairies
library(tidyverse)
library(trend)
library(sf)
library(scales)
library(ggplot2)
library(COGiter)
library(DT)
library(httr)#utiliser POST pour calcul i2m2 à partir du SEEE
library(ggrepel)
library(lubridate)
library(httr)

# On charge les fonctions utiles
functions <- list.files(path = "R",
                        pattern = ".R$",
                        full.names = TRUE)

map(.x = functions,
    .f = source)


Tendances_tax_groupe <-mk_st_by_group(abondance_relative,abondance_rel,GroupTaxo,GroupTaxo, code_station_hydrobio)
Tendances_tax <- mk_st_by_group(abondance_relative,abondance_rel,code_appel_taxon,code_appel_taxon, code_station_hydrobio)
Tendances_ept <- filter(Tendances_tax_groupe, GroupTaxo == "Trichoptera") %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_dipt <- filter(Tendances_tax_groupe, GroupTaxo == "Diptera") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_chiro <- filter(Tendances_tax, code_appel_taxon == "807") %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_gammare <- filter(Tendances_tax, code_appel_taxon %in% c("892","888","887")) %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_richesse <-mk_st_by_group(richesse_taxo,nb_taxons, nb_taxons, code_station_hydrobio)
Tendance_abondance_tot <- mk_st_by_group(abondance_relative, total_abondance, total_abondance, code_station_hydrobio)

# EPT 
ept <- abondance_relative %>% 
  filter(GroupTaxo == "Trichoptera")
ept_et_trend <- left_join(ept,Tendances_ept,by="code_station_hydrobio")

ept_et_trend <- ept_et_trend %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
    ),
    couleur = ifelse(mk_pvalue < 0.05, "darkblue", "white"),
    taille = rescale(abs(sens_slope), to = c(3, 8))  # ajuste selon ton besoin
  )


ept_et_trend_sf <- st_as_sf(ept_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)


ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ept_et_trend_sf) +
  geom_sf_text(
    data = ept_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  theme_minimal() +
  labs(title = "Tendance des indices EPT par station",
       subtitle = "Taille = pente | Couleur = significativité (p < 0.05)",
       caption = "▲ : tendance croissante, ▼ : décroissante, ● : aucune")

#Diptères 

dipt <- abondance_relative %>% 
  filter(GroupTaxo == "Diptera")
dipt_et_trend <- left_join(ept,Tendance_dipt,by="code_station_hydrobio")

dipt_et_trend <- dipt_et_trend %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
    ),
    couleur = ifelse(mk_pvalue < 0.05, "darkblue", "white"),
    taille = rescale(abs(sens_slope), to = c(3, 8))  # ajuste selon ton besoin
  )


dipt_et_trend_sf <- st_as_sf(dipt_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)


ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = dipt_et_trend_sf) +
  geom_sf_text(
    data = dipt_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  theme_minimal() +
  labs(title = "Tendance des abondances de diptères par station",
       subtitle = "Taille = pente | Couleur = significativité (p < 0.05)",
       caption = "▲ : tendance croissante, ▼ : décroissante, ● : aucune")




# Chiro
chiro <- abondance_relative %>% 
  filter(code_appel_taxon == "807")
chiro_et_trend <- left_join(chiro, Tendance_chiro, by = "code_station_hydrobio")

chiro_et_trend <- chiro_et_trend %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
    ),
    couleur = ifelse(mk_pvalue < 0.05, "darkblue", "white"),
    taille = rescale(abs(sens_slope), to = c(3, 8))  # ajuste selon ton besoin
  )


chiro_et_trend_sf <- st_as_sf(chiro_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)


ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = chiro_et_trend_sf) +
  geom_sf_text(
    data = chiro_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  theme_minimal() +
  labs(title = "Tendance des chiro par station",
       subtitle = "Taille = pente | Couleur = significativité (p < 0.05)",
       caption = "▲ : tendance croissante, ▼ : décroissante, ● : aucune")


#Gammares
gam <- abondance_relative %>% 
  filter(code_appel_taxon %in% c("892","888","887")) 
gam_et_trend <- left_join(gam,Tendance_gammare, by = "code_station_hydrobio")

gam_et_trend <- gam_et_trend %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
    ),
    couleur = ifelse(mk_pvalue < 0.05, "darkblue", "white"),
    taille = rescale(abs(sens_slope), to = c(3, 8))  # ajuste selon ton besoin
  )


gam_et_trend_sf <- st_as_sf(gam_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)

ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = gam_et_trend_sf) +
  geom_sf_text(
    data = gam_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  theme_minimal() +
  labs(title = "Tendance des Gammare par station",
       subtitle = "Taille = pente | Couleur = significativité (p < 0.05)",
       caption = "▲ : tendance croissante, ▼ : décroissante, ● : aucune")



#Richesse taxonomique
richesse_et_trend <- left_join(richesse_taxo, Tendance_richesse, by="code_station_hydrobio")

richesse_sf <- st_as_sf(richesse_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)

richesse_sf <- richesse_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
    ),
    couleur = ifelse(mk_pvalue < 0.05, "darkblue", "white"),
    taille = rescale(abs(sens_slope), to = c(3, 8))  # ajuste selon ton besoin
  )

ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = richesse_sf) +
  geom_sf_text(
    data = richesse_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  theme_minimal() +
  labs(title = "Tendance des richesse par station",
       subtitle = "Taille = pente | Couleur = significativité (p < 0.05)",
       caption = "▲ : tendance croissante, ▼ : décroissante, ● : aucune")

# Abondance totale 

abondance_et_trend <- left_join(abondance_relative, Tendance_abondance_tot, by="code_station_hydrobio")

abondance_sf <- st_as_sf(abondance_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)

abondance_sf <- abondance_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
    ),
    couleur = ifelse(mk_pvalue < 0.05, "darkblue", "white"),
    taille = rescale(abs(sens_slope), to = c(3, 8))  # ajuste selon ton besoin
  )

ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = abondance_sf) +
  geom_sf_text(
    data = abondance_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  theme_minimal() +
  labs(title = "Tendance des abondances totales par station",
       subtitle = "Taille = pente | Couleur = significativité (p < 0.05)",
       caption = "▲ : tendance croissante, ▼ : décroissante, ● : aucune")


# DATATABLE
ept_et_trend_sf_inc <- ept_et_trend_sf %>% 
  filter(trend == "Increase")
ept_et_trend_sf_dec <- ept_et_trend_sf %>% 
  filter(trend == "Decrease")

datatable(ept_et_trend_sf_inc, class = 'cell-border stripe',options =
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
            ),rownames=FALSE)

datatable(ept_et_trend_sf_dec, class = 'cell-border stripe',options =
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
            ),rownames=FALSE)


