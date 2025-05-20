load(file = "Data/80_donnees_globales_trans.rda")
library(car)
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(MASS)
library(dplyr)
library(lmtest)


# On renomme les variables
df_global <- df_global %>% 
  rename(I2M2 = `7613`,
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
         Corga = `1841`)

#On garde seulement les variables numériques
df_global_sans_stations <- df_global %>% 
  dplyr::select(`I2M2`:`Corga`) 


#On transforme les variables

#Transformation logarithmique
log_values <- log(df_global_sans_stations)

#Centrées-réduites
donnees_scale_global <- scale(df_global_sans_stations,center=TRUE,scale=TRUE)

#Racine carrée
df_sqrt <- sqrt(df_global_sans_stations)

#Arcsin
df_inverse <- 1/ df_global_sans_stations

#Données loggées et centrées réduites
donnees_scale_globa_log <- scale(log_values,center=TRUE,scale=TRUE)

#Transformation en df
df_donnees_scale_global <- as.data.frame(donnees_scale_global)
df_donnees_scale_global_log <- as.data.frame(donnees_scale_globa_log)


###################################################################################
#                                           Loi normale - variables non transformées  
###################################################################################


model_normal_non_transformées <- glm(`I2M2`~`O2dissous`+`NH4`+`NO3`+`Ptot`, data= df_global_sans_stations, family = gaussian(link = "identity"))
summary(model_normal_non_transformées)
model_simplifié <- MASS::stepAIC(model_normal_non_transformées)

bptest(model_normal_non_transformées, ~fitted(model_normal_non_transformées) + I(fitted(model_normal_non_transformées)^2)) # test de white, variance constante

plot(fitted(model_normal_non_transformées),residuals(model_normal_non_transformées,type="pearson"),
     xlab="valeurs prédites", ylab="résidus de pearson",
     main="résidus de pearson vs valeurs prédites")

vif(model_normal_non_transformées)

qqnorm(residuals(model_normal_non_transformées)) #normalité respectée 
qqline(residuals(model_normal_non_transformées))
hist(residuals(model_normal_non_transformées), breaks=30,
     main = "Distribution",
     xlab="Résidus")
shapiro.test(residuals(model_normal_non_transformées)) #si pvalue < 0,05 ce n'est pas normal

sjPlot::plot_model(model_normal_non_transformées, type = "pred",terms= ("NH4"), title = "Effets ammonium  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées, type = "pred",terms= ("DBO5"), title = "Effets DBO5  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées, type = "pred",terms= ("NO3"), title = "Effets des nitrates  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées, type = "pred",terms= ("Ptot"), title = "Effets de Ptot  sur l'I2M2")

plot(model_normal_non_transformées)

#dist_cook <- cooks.distance(model_normal_non_transformées)
#seuil_cook <- 4 / nrow(df_global_sans_stations)

#df_global_sans_stations_cook <- df_global_sans_stations %>% 
  cbind(dist_cook) %>% # ajout de la colonne avec les distances
  filter(dist_cook < seuil_cook) # suppression des observations avec distance > 4/N

#model_normal_non_transformées_cook <- glm(`I2M2`~`DBO5`+`O2dissous`+`PO4`+`NH4`+`turbidité`+`Temp`+`pH`+`conductiv`+`MES`+`SaturO2`+`NO2`+`NO3`+`Ptot`+`Corga`, data= df_global_sans_stations_cook, family = gaussian(link = "identity"))
#summary(model_normal_non_transformées_cook)
#model_simplifié_cook <- MASS::stepAIC(model_normal_non_transformées_cook)

#bptest(model_normal_non_transformées_cook, ~fitted(model_normal_non_transformées_cook) + I(fitted(model_normal_non_transformées_cook)^2)) # test de white, variance constante

#plot(fitted(model_normal_non_transformées_cook),residuals(model_normal_non_transformées_cook,type="pearson"),
     #xlab="valeurs prédites", ylab="résidus de pearson",
     #main="résidus de pearson vs valeurs prédites")

#vif(model_normal_non_transformées_cook)

#qqnorm(residuals(model_normal_non_transformées_cook))
#qqline(residuals(model_normal_non_transformées_cook))
#hist(residuals(model_normal_non_transformées_cook), breaks=30,
     #main = "Distribution",
     #xlab="Résidus")
#shapiro.test(residuals(model_normal_non_transformées_cook)) #si pvalue < 0,05 ce n'est pas normal

#plot(model_normal_non_transformées_cook)

#sjPlot::plot_model(model_normal_non_transformées_cook, type = "est", show.values = TRUE, title = "Effets des paramètres sur l'I2M2")
#sjPlot::plot_model(model_normal_non_transformées_cook, type = "pred",terms= c("O2dissous","NH4"), title = "Effets croisés oxygène-ammonium  sur l'I2M2")
#sjPlot::plot_model(model_normal_non_transformées_cook, type = "pred",terms= c("O2dissous","NO2"), title = "Effets croisés oxygène-nitrites  sur l'I2M2")
#sjPlot::plot_model(model_normal_non_transformées_cook, type = "pred",terms= c("O2dissous","Ptot"), title = "Effets croisés oxygène-Ptot  sur l'I2M2")
#sjPlot::plot_model(model_normal_non_transformées_cook, type = "pred",terms= c("O2dissous","PO4"), title = "Effets croisés oxygène-PO4  sur l'I2M2")



#################################################################################
#                                               Loi normale - valeurs centrées réduites  
#################################################################################


model_normal_cr <- glm(`I2M2`~`DBO5`+`O2dissous`+`PO4`+`NH4`+`turbidité`+`Temp`+`pH`+`conductiv`+`MES`+`SaturO2`+`NO2`+`NO3`+`Ptot`+`Corga`, data= df_donnees_scale_global, family = gaussian(link = "identity"))
model_simplifié_cr <- MASS::stepAIC(model_normal_cr)

bptest(model_normal_cr, ~fitted(model_normal_cr) + I(fitted(model_normal_cr)^2), data=df_donnees_scale_global) #variance constante
plot(fitted(model_normal_non_transformées),residuals(model_normal_non_transformées,type="pearson"),
     xlab="valeurs prédites", ylab="résidus de pearson",
     main="résidus de pearson vs valeurs prédites")
vif(model_normal_cr)


################################################################################
#                                  ASPT  - non transformées
################################################################################

model_normal_non_transformées_aspt <- glm(`ASPT`~`O2dissous`+`NH4`+`NO3`+`Ptot`+`pH`, data= df_global_sans_stations, family = gaussian(link = "identity")) #si on rajoute le pH hypothèses vérifiées, si loi Gamma sans pH hypothèses validées pour cette loi
summary(model_normal_non_transformées_aspt)
model_simplifié <- MASS::stepAIC(model_normal_non_transformées_aspt)

bptest(model_normal_non_transformées_aspt, ~fitted(model_normal_non_transformées_aspt) + I(fitted(model_normal_non_transformées_aspt)^2)) # test de white, variance non constante

plot(fitted(model_normal_non_transformées_aspt),residuals(model_normal_non_transformées_aspt,type="pearson"),
     xlab="valeurs prédites", ylab="résidus de pearson",
     main="résidus de pearson vs valeurs prédites")

vif(model_normal_non_transformées_aspt)

qqnorm(residuals(model_normal_non_transformées_aspt)) #normalité respectée 
qqline(residuals(model_normal_non_transformées_aspt))
hist(residuals(model_normal_non_transformées_aspt), breaks=30,
     main = "Distribution",
     xlab="Résidus")
shapiro.test(residuals(model_normal_non_transformées_aspt)) #si pvalue < 0,05 ce n'est pas normal

sjPlot::plot_model(model_normal_non_transformées_aspt, type = "pred",terms= ("NH4"), title = "Effets ammonium  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées_aspt, type = "pred",terms= ("O2dissous"), title = "Effets O2 dissous  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées_aspt, type = "pred",terms= ("NO3"), title = "Effets des nitrates  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées_aspt, type = "pred",terms= ("Ptot"), title = "Effets de Ptot  sur l'I2M2")

plot(model_normal_non_transformées_aspt)


#################################################################################
#                                               Polyvoltinisme - test  
#################################################################################

model_normal_non_transformées_pol <- glm(`polyvolI2M2`~`O2dissous`+`NH4`+`NO3`+`Ptot`, data=, family = Gamma(link = "identity")) #problème des notes = à 0
summary(model_normal_non_transformées_pol)summary(model_normal_non_transformées_pol)Gamma() 
model_simplifié <- MASS::stepAIC(model_normal_non_transformées_pol)

bptest(model_normal_non_transformées_pol, ~fitted(model_normal_non_transformées_pol) + I(fitted(model_normal_non_transformées_pol)^2))

plot(fitted(model_normal_non_transformées_pol),residuals(model_normal_non_transformées_pol,type="pearson"),
     xlab="valeurs prédites", ylab="résidus de pearson",
     main="résidus de pearson vs valeurs prédites")

vif(model_normal_non_transformées_pol)

qqnorm(residuals(model_normal_non_transformées_pol)) #normalité respectée 
qqline(residuals(model_normal_non_transformées_pol))
hist(residuals(model_normal_non_transformées_pol), breaks=30,
     main = "Distribution",
     xlab="Résidus")
shapiro.test(residuals(model_normal_non_transformées_pol)) #si pvalue < 0,05 ce n'est pas normal

sjPlot::plot_model(model_normal_non_transformées, type = "pred",terms= ("NH4"), title = "Effets ammonium  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées, type = "pred",terms= ("DBO5"), title = "Effets DBO5  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées, type = "pred",terms= ("NO3"), title = "Effets des nitrates  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées, type = "pred",terms= ("Ptot"), title = "Effets de Ptot  sur l'I2M2")

plot(model_normal_non_transformées)

#################################################################################
#                                               Ovoviviparité - test  
#################################################################################

model_normal_non_transformées_ov <- glm(`ovovivI2M2`~`O2dissous`+`NH4`+`NO3`+`Ptot`, data= df_global_sans_stations, family = gaussian(link = "identity")) 
summary(model_normal_non_transformées_ov)
model_simplifié <- MASS::stepAIC(model_normal_non_transformées_ov)

bptest(model_normal_non_transformées_ov, ~fitted(model_normal_non_transformées_ov) + I(fitted(model_normal_non_transformées_ov)^2)) # test de white, variance constante

plot(fitted(model_normal_non_transformées_ov),residuals(model_normal_non_transformées_ov,type="pearson"),
     xlab="valeurs prédites", ylab="résidus de pearson",
     main="résidus de pearson vs valeurs prédites")

vif(model_normal_non_transformées_ov)

qqnorm(residuals(model_normal_non_transformées_ov)) #normalité respectée 
qqline(residuals(model_normal_non_transformées_ov))
hist(residuals(model_normal_non_transformées_ov), breaks=30,
     main = "Distribution",
     xlab="Résidus")
shapiro.test(residuals(model_normal_non_transformées_ov)) #si pvalue < 0,05 ce n'est pas normal

sjPlot::plot_model(model_normal_non_transformées_ov, type = "pred",terms= ("NH4"), title = "Effets ammonium  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées_ov, type = "pred",terms= ("O2dissous"), title = "Effets DBO5  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées_ov, type = "pred",terms= ("NO3"), title = "Effets des nitrates  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées_ov, type = "pred",terms= ("Ptot"), title = "Effets de Ptot  sur l'I2M2")

plot(model_normal_non_transformées_ov)


#################################################################################
#                                               Richesse taxonomique - test  
#################################################################################

model_normal_non_transformées_ov <- glm(`richesI2M2`~`O2dissous`+`NH4`+`NO3`+`Ptot`, data= df_global_sans_stations, family = Gamma(link = "identity")) 
summary(model_normal_non_transformées_ov)
model_simplifié <- MASS::stepAIC(model_normal_non_transformées_ov)

bptest(model_normal_non_transformées_ov, ~fitted(model_normal_non_transformées_ov) + I(fitted(model_normal_non_transformées_ov)^2)) # test de white, variance constante

plot(fitted(model_normal_non_transformées_ov),residuals(model_normal_non_transformées_ov,type="pearson"),
     xlab="valeurs prédites", ylab="résidus de pearson",
     main="résidus de pearson vs valeurs prédites")

vif(model_normal_non_transformées_ov)

qqnorm(residuals(model_normal_non_transformées_ov)) #normalité respectée 
qqline(residuals(model_normal_non_transformées_ov))
hist(residuals(model_normal_non_transformées_ov), breaks=30,
     main = "Distribution",
     xlab="Résidus")
shapiro.test(residuals(model_normal_non_transformées_ov)) #si pvalue < 0,05 ce n'est pas normal

sjPlot::plot_model(model_normal_non_transformées_ov, type = "pred",terms= ("NH4"), title = "Effets ammonium  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées_ov, type = "pred",terms= ("O2dissous"), title = "Effets O2dissous sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées_ov, type = "pred",terms= ("NO3"), title = "Effets des nitrates  sur l'I2M2")
sjPlot::plot_model(model_normal_non_transformées_ov, type = "pred",terms= ("Ptot"), title = "Effets de Ptot  sur l'I2M2")

plot(model_normal_non_transformées_ov)


################################################################################
#                                  IBD  - non transformées
################################################################################

model_normal_non_transformées_ibd <- glm(`IBD`~`pH`+`NH4`+`NO3`+`Ptot`, data= df_global_sans_stations, family = gaussian(link = "identity"))
summary(model_normal_non_transformées_ibd)
model_simplifié <- MASS::stepAIC(model_normal_non_transformées_ibd)

bptest(model_normal_non_transformées_ibd, ~fitted(model_normal_non_transformées_ibd) + I(fitted(model_normal_non_transformées_ibd)^2)) # test de white, variance constante

plot(fitted(model_normal_non_transformées_ibd),residuals(model_normal_non_transformées_ibd,type="pearson"),
     xlab="valeurs prédites", ylab="résidus de pearson",
     main="résidus de pearson vs valeurs prédites")

vif(model_normal_non_transformées_ibd)

qqnorm(residuals(model_normal_non_transformées_ibd)) #normalité respectée 
qqline(residuals(model_normal_non_transformées_ibd))
hist(residuals(model_normal_non_transformées_ibd), breaks=30,
     main = "Distribution",
     xlab="Résidus")
shapiro.test(residuals(model_normal_non_transformées_ibd)) #si pvalue < 0,05 ce n'est pas normal

sjPlot::plot_model(model_normal_non_transformées_ibd, type = "pred",terms= ("NH4"), title = "Effets ammonium  sur l'IBD")
sjPlot::plot_model(model_normal_non_transformées_ibd, type = "pred",terms= ("NO3"), title = "Effets des nitrates  sur l'IBD")
sjPlot::plot_model(model_normal_non_transformées_ibd, type = "pred",terms= ("Ptot"), title = "Effets de Ptot  sur l'IBD")
sjPlot::plot_model(model_normal_non_transformées_ibd, type = "pred",terms= ("pH"), title = "Effets de Ptot  sur l'IBD")

plot(model_normal_non_transformées_ibd)


################################################################################
#                                  IBD  - loggées 
################################################################################

model_normal_log_ibd <- glm(`IBD`~`pH`+`NH4`+`NO3`+`Ptot`, data=log_values, family = gaussian(link = "identity"))
model_simplifié_log <- MASS::stepAIC(model_normal_log_ibd)

bptest(model_normal_log_ibd, ~fitted(model_normal_log_ibd) + I(fitted(model_normal_log_ibd)^2)) #variance non constante


################################################################################
#                                  IBD  - centrées-réduites 
################################################################################

model_normal_cr_ibd <- glm(`IBD`~`pH`+`NH4`+`NO3`+`Ptot`, data= df_donnees_scale_global, family = gaussian(link = "identity"))
model_simplifié_cr_ibd <- MASS::stepAIC(model_normal_cr_ibd)
bptest(model_normal_cr_ibd, ~fitted(model_normal_cr_ibd) + I(fitted(model_normal_cr_ibd)^2), data=df_donnees_scale_global) #variance constante
plot(fitted(model_normal_cr_ibd),residuals(model_normal_cr_ibd,type="pearson"),
     xlab="valeurs prédites", ylab="résidus de pearson",
     main="résidus de pearson vs valeurs prédites")
vif(model_normal_cr_ibd)
qqnorm(residuals(model_normal_cr_ibd))
shapiro.test(residuals(model_normal_cr_ibd)) #si pvalue < 0,05 ce n'est pas normal

sjPlot::plot_model(model_normal_cr_ibd, type = "pred",terms= ("NH4"), title = "Effets ammonium  sur l'IBD")
sjPlot::plot_model(model_normal_cr_ibd, type = "pred",terms= ("pH"), title = "Effets DBO5  sur l'IBD")
sjPlot::plot_model(model_normal_cr_ibd, type = "pred",terms= ("NO3"), title = "Effets des nitrates  sur l'IBD")
sjPlot::plot_model(model_normal_cr_ibd, type = "pred",terms= ("Ptot"), title = "Effets de Ptot  sur l'IBD")

plot(model_normal_cr_ibd)

