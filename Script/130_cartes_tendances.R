# Chargement des librairies et données

library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(ggspatial) 
load("Data/liste_taxon_ajout.Rdata")
load("Data/df_taxo.rda")
source(file = "R/mk_st_by_group.R")
source(file = "R/Mann_kendall_div.R")
functions <- list.files(path = "R",
                        pattern = ".R$",
                        full.names = TRUE)

map(.x = functions,
    .f = source)

# Création de la fonction
create_trend_map <- function(data_abondance_relative,
                             data_trends,
                             nom_col_filtre,
                             valeurs_filtre,
                             departement_breton_sf,
                             est_filtre_groupe_taxo = TRUE) {
  
  # Filtrer les données d'abondance en fonction de la colonne et des valeurs spécifiées
  if (est_filtre_groupe_taxo) {
    filtered_data <- data_abondance_relative %>%
      filter(GroupTaxo %in% valeurs_filtre)
  } else {
    filtered_data <- data_abondance_relative %>%
      filter(Cd_Taxon_norm %in% valeurs_filtre)
  }
  
  # Joindre avec les données de tendance
  data_avec_tendance <- left_join(filtered_data, data_trends, by = "code_station_hydrobio")
  
  # Convertir en objet sf
  data_sf <- st_as_sf(data_avec_tendance, coords = c("longitude", "latitude"), crs = 4326)
  
  # Mutate pour les symboles, couleurs et tailles
  data_sf <- data_sf %>%
    mutate(
      symbole = case_when(
        trend == "Increase" ~ "\u25B2",
        trend == "Decrease" ~ "\u25BC",
        TRUE ~ "\u25CF"
      ),
      couleur = case_when(
        trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
        trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
        TRUE ~ "black"
      ),
      taille = case_when(
        trend %in% c("Increase", "Decrease") ~ scales::rescale(abs(sens_slope), to = c(1, 6)),
        TRUE ~ 0.5
      )
    )
  
  # Créer la carte ggplot
  map_plot <- ggplot() +
    geom_sf(data = departement_breton_sf, fill = "gray95", color = "black", size = 0.3) +
    geom_sf(data = data_sf) +
    geom_sf_text(
      data = data_sf,
      aes(label = symbole, color = couleur, size = taille),
      show.legend = FALSE
    ) +
    scale_color_identity() +
    scale_size_identity() +
    annotation_scale(location = "br", line_width = .5) +
    annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
    theme_minimal() +
    labs(
      title = NULL,
      caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement"
    )
  
  return(map_plot)
}

# Calcul des tendances
Tendances_tax_groupe <- mk_st_by_group(abondance_relative, abondance_rel, GroupTaxo, GroupTaxo, code_station_hydrobio)
Tendances_tax <- mk_st_by_group(abondance_relative, abondance_rel, Cd_Taxon_norm, Cd_Taxon_norm, code_station_hydrobio)
Tendances_ept_eph <- filter(Tendances_tax_groupe, GroupTaxo == "Ephemeroptera") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendances_ept_ple <- filter(Tendances_tax_groupe, GroupTaxo == "Plecoptera") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendances_ept_tri <- filter(Tendances_tax_groupe, GroupTaxo == "Trichoptera") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_dipt <- filter(Tendances_tax_groupe, GroupTaxo == "Diptera") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_oligo <- filter(Tendances_tax_groupe, GroupTaxo == "Oligochaeta") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_chiro <- filter(Tendances_tax, Cd_Taxon_norm == "807") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_ase <- filter(Tendances_tax, Cd_Taxon_norm == "880") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_sipho <- filter(Tendances_tax, Cd_Taxon_norm == "174") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_gammare <- filter(Tendances_tax, Cd_Taxon_norm %in% c("892", "888", "887")) %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_amphi <- filter(Tendances_tax, Cd_Taxon_norm == "21") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_ber <- filter(Tendances_tax, Cd_Taxon_norm == "329") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_elec <- filter(Tendances_tax, Cd_Taxon_norm == "3181") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_habro <- filter(Tendances_tax, Cd_Taxon_norm == "491") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_chlo <- filter(Tendances_tax, Cd_Taxon_norm == "170") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_iso <- filter(Tendances_tax, Cd_Taxon_norm == "140") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_per <- filter(Tendances_tax, Cd_Taxon_norm == "150") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_dino <- filter(Tendances_tax, Cd_Taxon_norm == "156") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_proto <- filter(Tendances_tax, Cd_Taxon_norm == "46") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_richesse <- mk_st_by_group(richesse_taxo, nb_taxons, nb_taxons, code_station_hydrobio)
Tendance_abondance_tot <- mk_st_by_group(abondance_relative, total_abondance, total_abondance, code_station_hydrobio)

departement_breton <- departements_metro_geo %>%
  filter(DEP %in% c("22", "29", "35", "56")) %>%
  st_transform(crs = 4326)


# Trichoptera
map_trichoptera <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendances_ept_tri,
  nom_col_filtre = "GroupTaxo",
  valeurs_filtre = "Trichoptera",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = TRUE
)
print(map_trichoptera)

# Ephemeroptera
map_ephemeroptera <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendances_ept_eph,
  nom_col_filtre = "GroupTaxo",
  valeurs_filtre = "Ephemeroptera",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = TRUE
)
print(map_ephemeroptera)

# Plecoptera --> pas de tendance
map_plecoptera <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendances_ept_ple,
  nom_col_filtre = "GroupTaxo",
  valeurs_filtre = "Plecoptera",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = TRUE
)
print(map_plecoptera)

# Diptera --> pas de tendance
map_diptera <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_dipt,
  nom_col_filtre = "GroupTaxo",
  valeurs_filtre = "Diptera",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = TRUE
)
print(map_diptera)

# Oligochaeta --> quelques tendances
map_oligochaeta <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_oligo,
  nom_col_filtre = "GroupTaxo",
  valeurs_filtre = "Oligochaeta",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = TRUE
)
print(map_oligochaeta)

# Chironomes  --> quelques baisses à l'ouest et augmentation à l'est
map_chironomes <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_chiro,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "807",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_chironomes)

# Gammaridae  --> pas de tendance 
map_gammare <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_gammare,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = c("892", "888", "887"),
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_gammare)

# Asemmidae  --> qu'une baisse en Ille et vilaine 
map_asemidae <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_ase,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "880",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_asemidae)

#Siphonoperla --> problème de point ?  
map_siphonoperla <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_sipho,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "174",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_siphonoperla)

#Amphimoura

map_amphi <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_amphi,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "21",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_amphi)

#Beraeodes

map_ber <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_ber,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "329",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_ber)


#Electrogena 

map_elec <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_elec,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "3181",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_elec)

#Habrophlebia 

map_habro <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_habro,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "491",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_habro)


# Chloroperla 

map_chlo <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_chlo,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "170",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_chlo)


# Isoperla 

map_iso <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_iso,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "140",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_iso)


# Perlodes 

map_per <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_per,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "150",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_per)

#Dinocras 

map_dino <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_dino,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "156",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_dino)

#Protonemoura 

map_proto <- create_trend_map(
  data_abondance_relative = abondance_relative,
  data_trends = Tendance_proto,
  nom_col_filtre = "Cd_Taxon_norm",
  valeurs_filtre = "46",
  departement_breton_sf = departement_breton,
  est_filtre_groupe_taxo = FALSE
)
print(map_proto)



#Richesse et abondance 

# Autre fonction pour la richesse et l'abondance
create_overall_trend_map <- function(base_data_sf,
                                     departement_breton_sf) {
  
  data_sf <- st_as_sf(base_data_sf, coords = c("longitude", "latitude"), crs = 4326)
  
  data_sf <- data_sf %>%
    mutate(
      symbole = case_when(
        trend == "Increase" ~ "\u25B2",
        trend == "Decrease" ~ "\u25BC",
        TRUE ~ "\u25CF"
      ),
      couleur = case_when(
        trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
        trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
        TRUE ~ "black"
      ),
      taille = case_when(
        trend %in% c("Increase", "Decrease") ~ scales::rescale(abs(sens_slope), to = c(1, 6)),
        TRUE ~ 0.5
      )
    )
  
  map_plot <- ggplot() +
    geom_sf(data = departement_breton_sf, fill = "gray95", color = "black", size = 0.3) +
    geom_sf(data = data_sf) +
    geom_sf_text(
      data = data_sf,
      aes(label = symbole, color = couleur, size = taille),
      show.legend = FALSE
    ) +
    scale_color_identity() +
    scale_size_identity() +
    annotation_scale(location = "br", line_width = .5) +
    annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
    theme_minimal() +
    labs(
      title = NULL,
      caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement"
    )
  
  return(map_plot)
}

richesse_et_trend <- left_join(richesse_taxo, Tendance_richesse, by = "code_station_hydrobio")
map_richesse <- create_overall_trend_map(
  base_data_sf = richesse_et_trend,
  titre_carte = "Tendances de la Richesse Taxonomique",
  departement_breton_sf = departement_breton
)
print(map_richesse)

abondance_et_trend <- left_join(abondance_relative, Tendance_abondance_tot, by = "code_station_hydrobio")
map_abondance_totale <- create_overall_trend_map(
  base_data_sf = abondance_et_trend,
  titre_carte = "Tendances de l'Abondance Totale",
  departement_breton_sf = departement_breton
)
print(map_abondance_totale)