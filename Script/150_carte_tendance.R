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
library(ggspatial)

# On charge les fonctions utiles
functions <- list.files(path = "R",
                        pattern = ".R$",
                        full.names = TRUE)

map(.x = functions,
    .f = source)

Tendances_tax_groupe <-mk_st_by_group(abondance_relative,abondance_rel,GroupTaxo,GroupTaxo, code_station_hydrobio)
Tendances_tax <- mk_st_by_group(abondance_relative,abondance_rel,Cd_Taxon_norm,Cd_Taxon_norm, code_station_hydrobio)
Tendances_ept_eph <- filter(Tendances_tax_groupe, GroupTaxo == "Ephemeroptera") %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendances_ept_ple <- filter(Tendances_tax_groupe, GroupTaxo == "Plecoptera") %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendances_ept_tri <- filter(Tendances_tax_groupe, GroupTaxo == "Trichoptera") %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_dipt <- filter(Tendances_tax_groupe, GroupTaxo == "Diptera") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_oligo <- filter(Tendances_tax_groupe, GroupTaxo == "Oligochaeta") %>% dplyr::select(code_station_hydrobio, trend, sens_slope, mk_pvalue)
Tendance_chiro <- filter(Tendances_tax, Cd_Taxon_norm == "807") %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_ase <- filter(Tendances_tax, Cd_Taxon_norm == "880") %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_sipho <- filter(Tendances_tax, Cd_Taxon_norm == "174") %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)
Tendance_gammare <- filter(Tendances_tax, Cd_Taxon_norm %in% c("892","888","887")) %>% dplyr::select(code_station_hydrobio,trend,sens_slope,mk_pvalue)Tendance_richesse <-mk_st_by_group(richesse_taxo,nb_taxons, nb_taxons, code_station_hydrobio)
Tendance_abondance_tot <- mk_st_by_group(abondance_relative, total_abondance, total_abondance, code_station_hydrobio)


ept <- abondance_relative %>% 
  filter(GroupTaxo == "Trichoptera")

ept_et_trend <- left_join(ept,Tendances_ept_tri,by="code_station_hydrobio")

ept_et_trend_sf <- st_as_sf(ept_et_trend, coords = c("longitude", "latitude"), crs = 4326)

departement_breton <- departements_metro_geo %>% 
  filter(DEP %in% c("22","29","35","56")) %>% 
  st_transform(crs = 4326)

ept_et_trend_sf <- ept_et_trend_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )

# Tracer la carte avec geom_sf
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
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")


#Ephemeroptera 
ept_eph <- abondance_relative %>% 
  filter(GroupTaxo == "Ephemeroptera")

ept_et_trend_eph <- left_join(ept_eph,Tendances_ept_eph,by="code_station_hydrobio")

ept_et_trend_eph_sf <- st_as_sf(ept_et_trend_eph, coords = c("longitude", "latitude"), crs = 4326 )

ept_et_trend_eph_sf <- ept_et_trend_eph_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )


# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ept_et_trend_eph_sf) +
  geom_sf_text(
    data = ept_et_trend_eph_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")

#Plecoptère

ept_ple <- abondance_relative %>% 
  filter(GroupTaxo == "Plecoptera")
ept_et_trend_ple <- left_join(ept_ple,Tendances_ept_ple,by="code_station_hydrobio")
ept_et_trend_ple_sf <- st_as_sf(ept_et_trend_ple, coords = c("longitude", "latitude"), crs = 4326 )


ept_et_trend_ple_sf <- ept_et_trend_ple_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )


# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ept_et_trend_ple_sf) +
  geom_sf_text(
    data = ept_et_trend_ple_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")

#Diptera 

dipt <- abondance_relative %>% 
  filter(GroupTaxo == "Diptera")
dipt_et_trend <- left_join(ept,Tendance_dipt,by="code_station_hydrobio")
dipt_et_trend_sf <- st_as_sf(dipt_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

dipt_et_trend_sf <- dipt_et_trend_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )

# Tracer la carte avec geom_sf
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
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")


#Oligochete

oligo <- abondance_relative %>% 
  filter(GroupTaxo == "Oligochaeta")
oligo_et_trend <- left_join(oligo,Tendance_oligo,by="code_station_hydrobio")
oligo_et_trend_sf <- st_as_sf(oligo_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

oligo_et_trend_sf <- oligo_et_trend_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )

# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = oligo_et_trend_sf) +
  geom_sf_text(
    data = oligo_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")

#Chironomes

chiro <- abondance_relative %>% 
  filter(Cd_Taxon_norm == "807")
chiro_et_trend <- left_join(chiro, Tendance_chiro, by = "code_station_hydrobio")
chiro_et_trend_sf <- st_as_sf(chiro_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

chiro_et_trend_sf <- chiro_et_trend_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )

# Tracer la carte avec geom_sf
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
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")


#Gammares
gam <- abondance_relative %>% 
  filter(Cd_Taxon_norm %in% c("892","888","887"))
gam_et_trend <- left_join(gam,Tendance_gammare, by = "code_station_hydrobio")
gam_et_trend_sf <- st_as_sf(gam_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

gam_et_trend_sf <- gam_et_trend_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )

# Tracer la carte avec geom_sf
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
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")

#Richesse 

richesse_et_trend <- left_join(richesse_taxo, Tendance_richesse, by="code_station_hydrobio")

richesse_sf <- st_as_sf(richesse_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

richesse_sf <- richesse_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )

# Tracer la carte avec geom_sf
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
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")

#Abondance totale 

abondance_et_trend <- left_join(abondance_relative, Tendance_abondance_tot, by="code_station_hydrobio")

abondance_sf <- st_as_sf(abondance_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

abondance_sf <- abondance_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )

# Tracer la carte avec geom_sf
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
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")

#Asemmidae

ase <- abondance_relative %>% 
  filter(Cd_Taxon_norm == "880")
ase_et_trend <- left_join(ase,Tendance_ase, by = "code_station_hydrobio")
ase_et_trend_sf <- st_as_sf(ase_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

ase_et_trend_sf <- ase_et_trend_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )

# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ase_et_trend_sf) +
  geom_sf_text(
    data = ase_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")

# ase <- abondance_relative %>% 
filter(Cd_Taxon_norm == "880")
ase_et_trend <- left_join(ase,Tendance_ase, by = "code_station_hydrobio")
ase_et_trend_sf <- st_as_sf(ase_et_trend, coords = c("longitude", "latitude"), crs = 4326 )

ase_et_trend_sf <- ase_et_trend_sf %>%
  mutate(
    symbole = case_when(
      trend == "Increase" ~ "\u25B2",      # triangle vers le haut
      trend == "Decrease" ~ "\u25BC",    # triangle vers le bas
      
    ),
    couleur = case_when(
      trend == "Increase" & mk_pvalue < 0.05 ~ "#60BD68",
      trend == "Decrease" & mk_pvalue < 0.05 ~ "red",
      TRUE ~ "black"
    ),
    
    taille = case_when(
      trend %in% c("Increase", "Decrease") ~ rescale(abs(sens_slope), to = c(1, 6)),
      TRUE ~ 0.5
    )
  )

# Tracer la carte avec geom_sf
ggplot() +
  geom_sf(data = departement_breton, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(data = ase_et_trend_sf) +
  geom_sf_text(
    data = ase_et_trend_sf,
    aes(label = symbole, color = couleur, size = taille),
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_size_identity() +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  labs(caption = "▲ : augmentation, ▼ : baisse, ● : aucun changement")





