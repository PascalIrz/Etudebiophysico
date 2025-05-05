####################################################################################
# Script de test de la manière de calculer / agréger les indicateurs physico-chimiques par période de mois à mois
####################################################################################

library(tidyverse)
source (file = "R/calculer_indicateur.R")

# CONSTITUTION D'UN JEU DE DONNEES TEST

## Constitution des combinaisons de mois
# on veut toutes les combinaisons possibles entre un mois de début (entre 1 et 12) et un mois 
# de fin (entre le mois de début et 12)
combin_mois <- expand.grid(1:12, 1:12) %>%
  set_names("mois_debut", "mois_fin") %>%
  filter(mois_debut <= mois_fin)

# verification visuelle
combin_mois %>%
  mutate(remp = 1) %>%
  pivot_wider(names_from = mois_fin,
              values_from = remp)

# fixation de la graine du générateur de nombres aléatoires pour la fonction rnorm() ci-dessous
set.seed(seed = 123)

## assemblage du jeu de données
test_data <- expand.grid(c("station1", "station2"),
                         c("NO3", "PO4"),
                         2015:2024,
                         1:12) %>%
  set_names("station", "parametre", "annee", "mois") %>%
  mutate(
    concentration = rnorm(n = n(), mean = 8) +
      sin(mois / 12),
    station = as.character(station),
    parametre = as.character(parametre)
  )

glimpse(test_data)

# TEST DE LA FONCTION

# test simple
calculer_indicateur(
  .df = test_data,
  .mois_debut = 1,
  .mois_fin = 5,
  station,
  parametre
)

# test pour toutes les combinaisons de mois de début (.x) et de fin (.y)
resultat <- map2(
  .df = test_data,
  .f = calculer_indicateur,
  .x = combin_mois$mois_debut,
  .y = combin_mois$mois_fin,
  station,
  parametre
) %>%
  reduce(rbind)

# TEST DE LA FONCTION 2

# test simple
calculer_indicateur2(
  .df = test_data,
  .mois_debut = 1,
  .mois_fin = 5,
  .var_mois = mois,
  .var_valeur = concentration,
  station,
  parametre
)

# test pour toutes les combinaisons de mois de début (.x) et de fin (.y)
resultat2 <- map2_df(
  .df = test_data,
  .f = calculer_indicateur2,
  .var_mois = mois,
  .var_valeur = concentration,
  .x = combin_mois$mois_debut,
  .y = combin_mois$mois_fin,
  station,
  parametre
)



