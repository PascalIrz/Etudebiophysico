# Chargement des librairies et données

load(file = "Data/80_donnees_globales_trans.rda")
load(file = "Data/10_donnees_pretraitees.rda")
library(car)
library(MASS)
library(tidyverse)
library(sjPlot)
library(lmtest)

# On renomme les variables
df_global <- df_global %>%
  rename(
    I2M2 = `7613`,
    richesI2M2 = `8054`,
    ovovivI2M2 = `8055`,
    polyvolI2M2 = `8056`,
    ASPT = `8057`,
    ShannonI2M2 = `8058`,
    IPS = `1022`,
    IBD = `5856`,
    turbidité = `1295`,
    Temp = `1301`,
    pH = `1302`,
    conductiv = `1303`,
    MES = `1305`,
    O2dissous = `1311`,
    SaturO2 = `1312`,
    DBO5 = `1313`,
    NH4 = `1335`,
    NO2 = `1339`,
    NO3 = `1340`,
    Ptot = `1350`,
    PO4 = `1433`,
    Corga = `1841`
  )


# Utilisation de la fonction de calage du modèle

test <- caler_modele(df = df_global_sans_stations,
                     var_dependante = "ASPT")

mes_metriques <- names(df_global_sans_stations)[1:8]

res <- map_df(.x = mes_metriques,
              .f = caler_modele,
              df = df_global_sans_stations)

res_large <- res %>%
  pivot_wider(names_from = variable,
              values_from = coef)

analyser_modele(df = df_global_sans_stations,
                var_dependante = "ASPT")
