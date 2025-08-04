load(file = "Data/80_donnees_globales_trans.rda")
load(file = "Data/10_donnees_pretraitees.rda")
library(car)
library(tidyverse)
library(ggplot2)
library(MASS)
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

# Ajout de la longitude
stations_long1 <- clean_minv %>%
  dplyr::select(code_station_hydrobio, longitude) %>%
  distinct(code_station_hydrobio, longitude)

df_global_longitude <- df_global %>%
  left_join(stations_long1, by = "code_station_hydrobio")

#On garde seulement les variables numériques
df_global_sans_stations <- df_global %>%
  dplyr::select(I2M2:Corga)

df_global_sans_stations_longitude <- df_global_longitude %>%
  dplyr::select(I2M2:longitude)


###################################################################################
#                                           I2M2, chaque modèle
###################################################################################

# Modèle choisi
model_normal_non_transformées <-
  glm(
    `I2M2` ~ `O2dissous` + `NH4` + `NO3` + `Ptot`,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_normal_non_transformées)
model_simplifié <-
  MASS::stepAIC(model_normal_non_transformées)bptest(
    model_normal_non_transformées,
    ~ fitted(model_normal_non_transformées) + I(fitted(model_normal_non_transformées) ^
                                                  2)
  ) # test de white, variance constante

plot(
  fitted(model_normal_non_transformées),
  residuals(model_normal_non_transformées, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_normal_non_transformées)
qqnorm(residuals(model_normal_non_transformées)) #normalité respectée
qqline(residuals(model_normal_non_transformées))
hist(
  residuals(model_normal_non_transformées),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_normal_non_transformées)) #si pvalue < 0,05 ce n'est pas normal


plot(model_normal_non_transformées)


# Ajout nitrites
model_normal_non_transformées_nit <-
  glm(
    `I2M2` ~ `O2dissous` + `NO2` + `NO3` + `Ptot` + NO2,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_normal_non_transformées_nit)
model_simplifié <- MASS::stepAIC(model_normal_non_transformées_nit)
bptest(
  model_normal_non_transformées_nit,
  ~ fitted(model_normal_non_transformées_nit) + I(fitted(model_normal_non_transformées_nit) ^
                                                    2)
) # test de white, variance constante

plot(
  fitted(model_normal_non_transformées_nit),
  residuals(model_normal_non_transformées_nit, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_normal_non_transformées_nit)
qqnorm(residuals(model_normal_non_transformées_nit)) #normalité respectée
qqline(residuals(model_normal_non_transformées_nit))
hist(
  residuals(model_normal_non_transformées_nit),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_normal_non_transformées_nit)) #si pvalue < 0,05 ce n'est pas normal
plot(model_normal_non_transformées)

#Ajout pH
model_normal_non_transformées_ph <-
  glm(
    `I2M2` ~ `O2dissous` + `pH` + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_normal_non_transformées_ph)
model_simplifié <- MASS::stepAIC(model_normal_non_transformées_ph)
bptest(
  model_normal_non_transformées_ph,
  ~ fitted(model_normal_non_transformées_ph) + I(fitted(model_normal_non_transformées_ph) ^
                                                   2)
) # test de white, variance constante

plot(
  fitted(model_normal_non_transformées_ph),
  residuals(model_normal_non_transformées_ph, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_normal_non_transformées_ph)
qqnorm(residuals(model_normal_non_transformées_ph)) #normalité respectée
qqline(residuals(model_normal_non_transformées_ph))
hist(
  residuals(model_normal_non_transformées_ph),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_normal_non_transformées_ph)) #si pvalue < 0,05 ce n'est pas normal
plot(model_normal_non_transformées_ph)

# Ajout conductivité

model_normal_non_transformées_cond <-
  glm(
    `I2M2` ~ `O2dissous` + conductiv + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_normal_non_transformées_cond)
model_simplifié <- MASS::stepAIC(model_normal_non_transformées_cond)
bptest(
  model_normal_non_transformées_cond,
  ~ fitted(model_normal_non_transformées_cond) + I(fitted(model_normal_non_transformées_cond) ^
                                                     2)
) # test de white, variance constante

plot(
  fitted(model_normal_non_transformées_cond),
  residuals(model_normal_non_transformées_cond, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_normal_non_transformées_cond)
qqnorm(residuals(model_normal_non_transformées_cond)) #normalité respectée
qqline(residuals(model_normal_non_transformées_cond))
hist(
  residuals(model_normal_non_transformées_cond),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_normal_non_transformées_cond)) #si pvalue < 0,05 ce n'est pas normal
plot(model_normal_non_transformées_cond)


#################################################################################
#                                              ASPT
#################################################################################

# Ajout nitrites
model_ASPT_normal_non_transformées_nit <-
  glm(
    `ASPT` ~ `O2dissous` + `NO2` + `NO3` + `Ptot`,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_ASPT_normal_non_transformées_nit)
model_simplifié_nit <-
  MASS::stepAIC(model_ASPT_normal_non_transformées_nit)
bptest(
  model_ASPT_normal_non_transformées_nit,
  ~ fitted(model_ASPT_normal_non_transformées_nit) + I(fitted(model_ASPT_normal_non_transformées_nit) ^
                                                         2)
) # test de white, variance constante

plot(
  fitted(model_ASPT_normal_non_transformées_nit),
  residuals(model_ASPT_normal_non_transformées_nit, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_ASPT_normal_non_transformées_nit)
qqnorm(residuals(model_ASPT_normal_non_transformées_nit)) #normalité respectée
qqline(residuals(model_ASPT_normal_non_transformées_nit))
hist(
  residuals(model_ASPT_normal_non_transformées_nit),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_ASPT_normal_non_transformées_nit)) #si pvalue < 0,05 ce n'est pas normal
plot(model_ASPT_normal_non_transformées_nit)

# Ajout pH
model_ASPT_normal_non_transformées_ph <-
  glm(
    `ASPT` ~ `O2dissous` + `pH` + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_ASPT_normal_non_transformées_ph)
model_simplifié_ph <-
  MASS::stepAIC(model_ASPT_normal_non_transformées_ph)
bptest(
  model_ASPT_normal_non_transformées_ph,
  ~ fitted(model_ASPT_normal_non_transformées_ph) + I(fitted(model_ASPT_normal_non_transformées_ph) ^
                                                        2)
) # test de white, variance constante

plot(
  fitted(model_ASPT_normal_non_transformées_ph),
  residuals(model_ASPT_normal_non_transformées_ph, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_ASPT_normal_non_transformées_ph)
qqnorm(residuals(model_ASPT_normal_non_transformées_ph)) #normalité respectée
qqline(residuals(model_ASPT_normal_non_transformées_ph))
hist(
  residuals(model_ASPT_normal_non_transformées_ph),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_ASPT_normal_non_transformées_ph)) #si pvalue < 0,05 ce n'est pas normal


# Ajout conductivité
model_ASPT_normal_non_transformées_cond <-
  glm(
    `ASPT` ~ `O2dissous` + conductiv + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_ASPT_normal_non_transformées_cond)
model_simplifié_cond <-
  MASS::stepAIC(model_ASPT_normal_non_transformées_cond)
bptest(
  model_ASPT_normal_non_transformées_cond,
  ~ fitted(model_ASPT_normal_non_transformées_cond) + I(fitted(
    model_ASPT_normal_non_transformées_cond
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_ASPT_normal_non_transformées_cond),
  residuals(model_ASPT_normal_non_transformées_cond, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_ASPT_normal_non_transformées_cond)
qqnorm(residuals(model_ASPT_normal_non_transformées_cond)) #normalité respectée
qqline(residuals(model_ASPT_normal_non_transformées_cond))
hist(
  residuals(model_ASPT_normal_non_transformées_cond),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_ASPT_normal_non_transformées_cond)) #si pvalue < 0,05 ce n'est pas normal
plot(model_ASPT_normal_non_transformées_cond)

###################################################################################
#                                           Ovoviviparité
###################################################################################

# Ajout nitrites
model_OvovivI2M2_normal_non_transformées_nit <-
  glm(
    `ovovivI2M2` ~ `O2dissous` + `NO2` + `NO3` + `Ptot`,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_OvovivI2M2_normal_non_transformées_nit)
model_simplifié_nit <-
  MASS::stepAIC(model_OvovivI2M2_normal_non_transformées_nit)
bptest(
  model_OvovivI2M2_normal_non_transformées_nit,
  ~ fitted(model_OvovivI2M2_normal_non_transformées_nit) + I(fitted(
    model_OvovivI2M2_normal_non_transformées_nit
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_OvovivI2M2_normal_non_transformées_nit),
  residuals(model_OvovivI2M2_normal_non_transformées_nit, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_OvovivI2M2_normal_non_transformées_nit)
qqnorm(residuals(model_OvovivI2M2_normal_non_transformées_nit)) #normalité respectée
qqline(residuals(model_OvovivI2M2_normal_non_transformées_nit))
hist(
  residuals(model_OvovivI2M2_normal_non_transformées_nit),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_OvovivI2M2_normal_non_transformées_nit)) #si pvalue < 0,05 ce n'est pas normal---# Ajout pH
model_OvovivI2M2_normal_non_transformées_ph <-
  glm(
    `ovovivI2M2` ~ `O2dissous` + `pH` + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_OvovivI2M2_normal_non_transformées_ph)
model_simplifié_ph <-
  MASS::stepAIC(model_OvovivI2M2_normal_non_transformées_ph)
bptest(
  model_OvovivI2M2_normal_non_transformées_ph,
  ~ fitted(model_OvovivI2M2_normal_non_transformées_ph) + I(fitted(
    model_OvovivI2M2_normal_non_transformées_ph
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_OvovivI2M2_normal_non_transformées_ph),
  residuals(model_OvovivI2M2_normal_non_transformées_ph, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_OvovivI2M2_normal_non_transformées_ph)
qqnorm(residuals(model_OvovivI2M2_normal_non_transformées_ph)) #normalité respectée
qqline(residuals(model_OvovivI2M2_normal_non_transformées_ph))
hist(
  residuals(model_OvovivI2M2_normal_non_transformées_ph),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_OvovivI2M2_normal_non_transformées_ph)) #si pvalue < 0,05 ce n'est pas normal
plot(model_OvovivI2M2_normal_non_transformées_ph)

---# Ajout conductivité
  model_OvovivI2M2_normal_non_transformées_cond <-
  glm(
    `ovovivI2M2` ~ `O2dissous` + conductiv + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_OvovivI2M2_normal_non_transformées_cond)
model_simplifié_cond <-
  MASS::stepAIC(model_OvovivI2M2_normal_non_transformées_cond)
bptest(
  model_OvovivI2M2_normal_non_transformées_cond,
  ~ fitted(model_OvovivI2M2_normal_non_transformées_cond) + I(fitted(
    model_OvovivI2M2_normal_non_transformées_cond
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_OvovivI2M2_normal_non_transformées_cond),
  residuals(model_OvovivI2M2_normal_non_transformées_cond, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_OvovivI2M2_normal_non_transformées_cond)
qqnorm(residuals(model_OvovivI2M2_normal_non_transformées_cond)) #normalité respectée
qqline(residuals(model_OvovivI2M2_normal_non_transformées_cond))
hist(
  residuals(model_OvovivI2M2_normal_non_transformées_cond),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_OvovivI2M2_normal_non_transformées_cond)) #si pvalue < 0,05 ce n'est pas normal
plot(model_OvovivI2M2_normal_non_transformées_cond)

###################################################################################
#                                           Polyvoltinisme
###################################################################################


# Ajout nitrites
model_PolyvolI2M2_normal_non_transformées_nit <-
  glm(
    `polyvolI2M2` ~ `O2dissous` + `NO2` + `NO3` + `Ptot`,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_PolyvolI2M2_normal_non_transformées_nit)
model_simplifié_nit <-
  MASS::stepAIC(model_PolyvolI2M2_normal_non_transformées_nit)
bptest(
  model_PolyvolI2M2_normal_non_transformées_nit,
  ~ fitted(model_PolyvolI2M2_normal_non_transformées_nit) + I(fitted(
    model_PolyvolI2M2_normal_non_transformées_nit
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_PolyvolI2M2_normal_non_transformées_nit),
  residuals(model_PolyvolI2M2_normal_non_transformées_nit, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_PolyvolI2M2_normal_non_transformées_nit)
qqnorm(residuals(model_PolyvolI2M2_normal_non_transformées_nit)) #normalité respectée
qqline(residuals(model_PolyvolI2M2_normal_non_transformées_nit))
hist(
  residuals(model_PolyvolI2M2_normal_non_transformées_nit),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_PolyvolI2M2_normal_non_transformées_nit)) #si pvalue < 0,05 ce n'est pas normal
plot(model_PolyvolI2M2_normal_non_transformées_nit)

---# Ajout pH
  model_PolyvolI2M2_normal_non_transformées_ph <-
  glm(
    `polyvolI2M2` ~ `O2dissous` + `pH` + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_PolyvolI2M2_normal_non_transformées_ph)
model_simplifié_ph <-
  MASS::stepAIC(model_PolyvolI2M2_normal_non_transformées_ph)
bptest(
  model_PolyvolI2M2_normal_non_transformées_ph,
  ~ fitted(model_PolyvolI2M2_normal_non_transformées_ph) + I(fitted(
    model_PolyvolI2M2_normal_non_transformées_ph
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_PolyvolI2M2_normal_non_transformées_ph),
  residuals(model_PolyvolI2M2_normal_non_transformées_ph, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_PolyvolI2M2_normal_non_transformées_ph)
qqnorm(residuals(model_PolyvolI2M2_normal_non_transformées_ph)) #normalité respectée
qqline(residuals(model_PolyvolI2M2_normal_non_transformées_ph))
hist(
  residuals(model_PolyvolI2M2_normal_non_transformées_ph),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_PolyvolI2M2_normal_non_transformées_ph)) #si pvalue < 0,05 ce n'est pas normal
plot(model_PolyvolI2M2_normal_non_transformées_ph)

---# Ajout conductivité
  model_PolyvolI2M2_normal_non_transformées_cond <-
  glm(
    `polyvolI2M2` ~ `O2dissous` + conductiv + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_PolyvolI2M2_normal_non_transformées_cond)
model_simplifié_cond <-
  MASS::stepAIC(model_PolyvolI2M2_normal_non_transformées_cond)
bptest(
  model_PolyvolI2M2_normal_non_transformées_cond,
  ~ fitted(model_PolyvolI2M2_normal_non_transformées_cond) + I(fitted(
    model_PolyvolI2M2_normal_non_transformées_cond
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_PolyvolI2M2_normal_non_transformées_cond),
  residuals(model_PolyvolI2M2_normal_non_transformées_cond, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_PolyvolI2M2_normal_non_transformées_cond)
qqnorm(residuals(model_PolyvolI2M2_normal_non_transformées_cond)) #normalité respectée
qqline(residuals(model_PolyvolI2M2_normal_non_transformées_cond))
hist(
  residuals(model_PolyvolI2M2_normal_non_transformées_cond),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_PolyvolI2M2_normal_non_transformées_cond)) #si pvalue < 0,05 ce n'est pas normal
plot(model_PolyvolI2M2_normal_non_transformées_cond)

###################################################################################
#                                           Indice de Shannon
###################################################################################

# Ajout nitrites
model_shannonI2M2_normal_non_transformées_nit <-
  glm(
    `shannonI2M2` ~ `O2dissous` + `NO2` + `NO3` + `Ptot`,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_shannonI2M2_normal_non_transformées_nit)
model_simplifié_nit <-
  MASS::stepAIC(model_shannonI2M2_normal_non_transformées_nit)
bptest(
  model_shannonI2M2_normal_non_transformées_nit,
  ~ fitted(model_shannonI2M2_normal_non_transformées_nit) + I(fitted(
    model_shannonI2M2_normal_non_transformées_nit
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_shannonI2M2_normal_non_transformées_nit),
  residuals(model_shannonI2M2_normal_non_transformées_nit, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_shannonI2M2_normal_non_transformées_nit)
qqnorm(residuals(model_shannonI2M2_normal_non_transformées_nit)) #normalité respectée
qqline(residuals(model_shannonI2M2_normal_non_transformées_nit))
hist(
  residuals(model_shannonI2M2_normal_non_transformées_nit),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_shannonI2M2_normal_non_transformées_nit)) #si pvalue < 0,05 ce n'est pas normal
plot(model_shannonI2M2_normal_non_transformées_nit)

---# Ajout pH
  model_shannonI2M2_normal_non_transformées_ph <-
  glm(
    `shannonI2M2` ~ `O2dissous` + `pH` + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_shannonI2M2_normal_non_transformées_ph)
model_simplifié_ph <-
  MASS::stepAIC(model_shannonI2M2_normal_non_transformées_ph)
bptest(
  model_shannonI2M2_normal_non_transformées_ph,
  ~ fitted(model_shannonI2M2_normal_non_transformées_ph) + I(fitted(
    model_shannonI2M2_normal_non_transformées_ph
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_shannonI2M2_normal_non_transformées_ph),
  residuals(model_shannonI2M2_normal_non_transformées_ph, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_shannonI2M2_normal_non_transformées_ph)
qqnorm(residuals(model_shannonI2M2_normal_non_transformées_ph)) #normalité respectée
qqline(residuals(model_shannonI2M2_normal_non_transformées_ph))
hist(
  residuals(model_shannonI2M2_normal_non_transformées_ph),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_shannonI2M2_normal_non_transformées_ph)) #si pvalue < 0,05 ce n'est pas normal
plot(model_shannonI2M2_normal_non_transformées_ph)

---# Ajout conductivité
  model_shannonI2M2_normal_non_transformées_cond <-
  glm(
    `shannonI2M2` ~ `O2dissous` + conductiv + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_shannonI2M2_normal_non_transformées_cond)
model_simplifié_cond <-
  MASS::stepAIC(model_shannonI2M2_normal_non_transformées_cond)
bptest(
  model_shannonI2M2_normal_non_transformées_cond,
  ~ fitted(model_shannonI2M2_normal_non_transformées_cond) + I(fitted(
    model_shannonI2M2_normal_non_transformées_cond
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_shannonI2M2_normal_non_transformées_cond),
  residuals(model_shannonI2M2_normal_non_transformées_cond, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_shannonI2M2_normal_non_transformées_cond)
qqnorm(residuals(model_shannonI2M2_normal_non_transformées_cond)) #normalité respectée
qqline(residuals(model_shannonI2M2_normal_non_transformées_cond))
hist(
  residuals(model_shannonI2M2_normal_non_transformées_cond),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_shannonI2M2_normal_non_transformées_cond)) #si pvalue < 0,05 ce n'est pas normal
plot(model_shannonI2M2_normal_non_transformées_cond)

###################################################################################
#                                           Richesse taxonomique
###################################################################################

# Ajout nitrites
model_RichesI2M2_normal_non_transformées_nit <-
  glm(
    `richesI2M2` ~ `O2dissous` + `NO2` + `NO3` + `Ptot`,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_RichesI2M2_normal_non_transformées_nit)
model_simplifié_nit <-
  MASS::stepAIC(model_RichesI2M2_normal_non_transformées_nit)
bptest(
  model_RichesI2M2_normal_non_transformées_nit,
  ~ fitted(model_RichesI2M2_normal_non_transformées_nit) + I(fitted(
    model_RichesI2M2_normal_non_transformées_nit
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_RichesI2M2_normal_non_transformées_nit),
  residuals(model_RichesI2M2_normal_non_transformées_nit, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_RichesI2M2_normal_non_transformées_nit)
qqnorm(residuals(model_RichesI2M2_normal_non_transformées_nit)) #normalité respectée
qqline(residuals(model_RichesI2M2_normal_non_transformées_nit))
hist(
  residuals(model_RichesI2M2_normal_non_transformées_nit),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_RichesI2M2_normal_non_transformées_nit)) #si pvalue < 0,05 ce n'est pas normal
plot(model_RichesI2M2_normal_non_transformées_nit)

---# Ajout pH
  model_RichesI2M2_normal_non_transformées_ph <-
  glm(
    `richesI2M2` ~ `O2dissous` + `pH` + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_RichesI2M2_normal_non_transformées_ph)
model_simplifié_ph <-
  MASS::stepAIC(model_RichesI2M2_normal_non_transformées_ph)
bptest(
  model_RichesI2M2_normal_non_transformées_ph,
  ~ fitted(model_RichesI2M2_normal_non_transformées_ph) + I(fitted(
    model_RichesI2M2_normal_non_transformées_ph
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_RichesI2M2_normal_non_transformées_ph),
  residuals(model_RichesI2M2_normal_non_transformées_ph, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_RichesI2M2_normal_non_transformées_ph)
qqnorm(residuals(model_RichesI2M2_normal_non_transformées_ph)) #normalité respectée
qqline(residuals(model_RichesI2M2_normal_non_transformées_ph))
hist(
  residuals(model_RichesI2M2_normal_non_transformées_ph),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_RichesI2M2_normal_non_transformées_ph)) #si pvalue < 0,05 ce n'est pas normal
plot(model_RichesI2M2_normal_non_transformées_ph)

---# Ajout conductivité
  model_RichesI2M2_normal_non_transformées_cond <-
  glm(
    `richesI2M2` ~ `O2dissous` + conductiv + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_RichesI2M2_normal_non_transformées_cond)
model_simplifié_cond <-
  MASS::stepAIC(model_RichesI2M2_normal_non_transformées_cond)
bptest(
  model_RichesI2M2_normal_non_transformées_cond,
  ~ fitted(model_RichesI2M2_normal_non_transformées_cond) + I(fitted(
    model_RichesI2M2_normal_non_transformées_cond
  ) ^ 2)
) # test de white, variance constante

plot(
  fitted(model_RichesI2M2_normal_non_transformées_cond),
  residuals(model_RichesI2M2_normal_non_transformées_cond, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_RichesI2M2_normal_non_transformées_cond)
qqnorm(residuals(model_RichesI2M2_normal_non_transformées_cond)) #normalité respectée
qqline(residuals(model_RichesI2M2_normal_non_transformées_cond))
hist(
  residuals(model_RichesI2M2_normal_non_transformées_cond),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_RichesI2M2_normal_non_transformées_cond)) #si pvalue < 0,05 ce n'est pas normal
plot(model_RichesI2M2_normal_non_transformées_cond)

###################################################################################
#                                           IBD
###################################################################################

# Ajout nitrites
model_IBD_normal_non_transformées_nit <-
  glm(
    `IBD` ~ `O2dissous` + `NO2` + `NO3` + `Ptot`,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_IBD_normal_non_transformées_nit)
model_simplifié_nit <-
  MASS::stepAIC(model_IBD_normal_non_transformées_nit)
bptest(
  model_IBD_normal_non_transformées_nit,
  ~ fitted(model_IBD_normal_non_transformées_nit) + I(fitted(model_IBD_normal_non_transformées_nit) ^
                                                        2)
) # test de white, variance constante

plot(
  fitted(model_IBD_normal_non_transformées_nit),
  residuals(model_IBD_normal_non_transformées_nit, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_IBD_normal_non_transformées_nit)
qqnorm(residuals(model_IBD_normal_non_transformées_nit)) #normalité respectée
qqline(residuals(model_IBD_normal_non_transformées_nit))
hist(
  residuals(model_IBD_normal_non_transformées_nit),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_IBD_normal_non_transformées_nit)) #si pvalue < 0,05 ce n'est pas normal
plot(model_IBD_normal_non_transformées_nit)

---# Ajout pH
  model_IBD_normal_non_transformées_ph <-
  glm(
    `IBD` ~ `O2dissous` + `pH` + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_IBD_normal_non_transformées_ph)
model_simplifié_ph <-
  MASS::stepAIC(model_IBD_normal_non_transformées_ph)
bptest(
  model_IBD_normal_non_transformées_ph,
  ~ fitted(model_IBD_normal_non_transformées_ph) + I(fitted(model_IBD_normal_non_transformées_ph) ^
                                                       2)
) # test de white, variance constante

plot(
  fitted(model_IBD_normal_non_transformées_ph),
  residuals(model_IBD_normal_non_transformées_ph, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_IBD_normal_non_transformées_ph)
qqnorm(residuals(model_IBD_normal_non_transformées_ph)) #normalité respectée
qqline(residuals(model_IBD_normal_non_transformées_ph))
hist(
  residuals(model_IBD_normal_non_transformées_ph),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_IBD_normal_non_transformées_ph)) #si pvalue < 0,05 ce n'est pas normal
plot(model_IBD_normal_non_transformées_ph)

---# Ajout conductivité
  model_IBD_normal_non_transformées_cond <-
  glm(
    `IBD` ~ `O2dissous` + conductiv + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_IBD_normal_non_transformées_cond)
model_simplifié_cond <-
  MASS::stepAIC(model_IBD_normal_non_transformées_cond)
bptest(
  model_IBD_normal_non_transformées_cond,
  ~ fitted(model_IBD_normal_non_transformées_cond) + I(fitted(model_IBD_normal_non_transformées_cond) ^
                                                         2)
) # test de white, variance constante

plot(
  fitted(model_IBD_normal_non_transformées_cond),
  residuals(model_IBD_normal_non_transformées_cond, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_IBD_normal_non_transformées_cond)
qqnorm(residuals(model_IBD_normal_non_transformées_cond)) #normalité respectée
qqline(residuals(model_IBD_normal_non_transformées_cond))
hist(
  residuals(model_IBD_normal_non_transformées_cond),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_IBD_normal_non_transformées_cond)) #si pvalue < 0,05 ce n'est pas normal
plot(model_IBD_normal_non_transformées_cond)

###################################################################################
#                                           IPS
###################################################################################

# Ajout nitrites
model_IPS_normal_non_transformées_nit <-
  glm(
    `IPS` ~ `O2dissous` + `NO2` + `NO3` + `Ptot`,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_IPS_normal_non_transformées_nit)
model_simplifié_nit <-
  MASS::stepAIC(model_IPS_normal_non_transformées_nit)
bptest(
  model_IPS_normal_non_transformées_nit,
  ~ fitted(model_IPS_normal_non_transformées_nit) + I(fitted(model_IPS_normal_non_transformées_nit) ^
                                                        2)
) # test de white, variance constante

plot(
  fitted(model_IPS_normal_non_transformées_nit),
  residuals(model_IPS_normal_non_transformées_nit, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_IPS_normal_non_transformées_nit)
qqnorm(residuals(model_IPS_normal_non_transformées_nit)) #normalité respectée
qqline(residuals(model_IPS_normal_non_transformées_nit))
hist(
  residuals(model_IPS_normal_non_transformées_nit),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_IPS_normal_non_transformées_nit)) #si pvalue < 0,05 ce n'est pas normal
plot(model_IPS_normal_non_transformées_nit)

---# Ajout pH
  model_IPS_normal_non_transformées_ph <-
  glm(
    `IPS` ~ `O2dissous` + `pH` + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_IPS_normal_non_transformées_ph)
model_simplifié_ph <-
  MASS::stepAIC(model_IPS_normal_non_transformées_ph)
bptest(
  model_IPS_normal_non_transformées_ph,
  ~ fitted(model_IPS_normal_non_transformées_ph) + I(fitted(model_IPS_normal_non_transformées_ph) ^
                                                       2)
) # test de white, variance constante

plot(
  fitted(model_IPS_normal_non_transformées_ph),
  residuals(model_IPS_normal_non_transformées_ph, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_IPS_normal_non_transformées_ph)
qqnorm(residuals(model_IPS_normal_non_transformées_ph)) #normalité respectée
qqline(residuals(model_IPS_normal_non_transformées_ph))
hist(
  residuals(model_IPS_normal_non_transformées_ph),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_IPS_normal_non_transformées_ph)) #si pvalue < 0,05 ce n'est pas normal
plot(model_IPS_normal_non_transformées_ph)

---# Ajout conductivité
  model_IPS_normal_non_transformées_cond <-
  glm(
    `IPS` ~ `O2dissous` + conductiv + `NO3` + `Ptot` + NH4,
    data = df_global_sans_stations,
    family = gaussian(link = "identity")
  )
summary(model_IPS_normal_non_transformées_cond)
model_simplifié_cond <-
  MASS::stepAIC(model_IPS_normal_non_transformées_cond)
bptest(
  model_IPS_normal_non_transformées_cond,
  ~ fitted(model_IPS_normal_non_transformées_cond) + I(fitted(model_IPS_normal_non_transformées_cond) ^
                                                         2)
) # test de white, variance constante

plot(
  fitted(model_IPS_normal_non_transformées_cond),
  residuals(model_IPS_normal_non_transformées_cond, type = "pearson"),
  xlab = "valeurs prédites",
  ylab = "résidus de pearson",
  main = "résidus de pearson vs valeurs prédites"
)

vif(model_IPS_normal_non_transformées_cond)
qqnorm(residuals(model_IPS_normal_non_transformées_cond)) #normalité respectée
qqline(residuals(model_IPS_normal_non_transformées_cond))
hist(
  residuals(model_IPS_normal_non_transformées_cond),
  breaks = 30,
  main = "Distribution",
  xlab = "Résidus"
)
shapiro.test(residuals(model_IPS_normal_non_transformées_cond)) #si pvalue < 0,05 ce n'est pas normal
plot(model_IPS_normal_non_transformées_cond)
