#Chargement des données 
load(file = "Data/10_donnees_pretraitees.rda")

#Librairies
library(tidyverse)

# Vérification de la fraction analysée pour les nitrates 
verif_fraction <- parametres_physico %>%
  select(code_parametre,code_station_hydrobio,date_prelevement,code_fraction, resultat) %>% 
  filter(code_parametre==1340) %>% 
  arrange(code_station_hydrobio)

date_fraction <- verif_fraction %>% 
  group_by(date_prelevement, code_station_hydrobio) %>%
  summarise(nb_fraction = n_distinct(code_fraction), .groups = "drop") %>%
  filter(nb_fraction > 1)
  

verif_fraction <- verif_fraction %>%
  group_by(date_prelevement, code_station_hydrobio) %>%
  mutate(
    score = case_when(
      n_distinct(code_fraction) > 1 ~ 3,
      code_fraction == 3 ~ 1,
      code_fraction == 23 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


ggplot(verif_fraction,
       aes(x = as.factor(date_prelevement), 
           y = factor(code_station_hydrobio),
           fill = as.factor(score))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "red","3"="violet")) +
  labs(
    title = "Méthode d'analyse Nitrates",
    x = "Date",
    y = "Station",
    fill = "Score"
  ) +
  theme_classic()

which(is.na(verif_fraction))
 
#Pour l'ammonium
verif_fraction_ammonium<- parametres_physico %>%
  select(code_parametre,code_station_hydrobio,date_prelevement,code_fraction, resultat) %>% 
  filter(code_parametre==1335) %>% 
  arrange(code_station_hydrobio)



verif_fraction_ammonium <- verif_fraction_ammonium %>%
  group_by(date_prelevement, code_station_hydrobio) %>%
  mutate(
    score = case_when(
      n_distinct(code_fraction) > 1 ~ 3,
      code_fraction == 3 ~ 1,
      code_fraction == 23 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


ggplot(verif_fraction_ammonium,
       aes(x = as.factor(date_prelevement), 
           y = factor(code_station_hydrobio),
           fill = as.factor(score))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "red","3"="violet")) +
  labs(
    title = "Méthode d'analyse Ammonium",
    x = "Date",
    y = "Station",
    fill = "Score"
  ) +
  theme_classic()



#Pour les nitrites
verif_fraction_nitrites <- parametres_physico %>%
  select(code_parametre,code_station_hydrobio,date_prelevement,code_fraction, resultat) %>% 
  filter(code_parametre==1339) %>% 
  arrange(code_station_hydrobio)


verif_fraction_nitrites <- verif_fraction_nitrites %>%
  group_by(date_prelevement, code_station_hydrobio) %>%
  mutate(
    score = case_when(
      n_distinct(code_fraction) > 1 ~ 3,
      code_fraction == 3 ~ 1,
      code_fraction == 23 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


ggplot(verif_fraction_nitrites,
       aes(x = as.factor(date_prelevement), 
           y = factor(code_station_hydrobio),
           fill = as.factor(score))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "red","3"="violet")) +
  labs(
    title = "Méthode d'analyse Nitrites",
    x = "Date",
    y = "Station",
    fill = "Score"
  ) +
  theme_classic()

which(is.na(verif_fraction))

#Pour les orthophosphates
verif_fraction_po4 <- parametres_physico %>%
  select(code_parametre,code_station_hydrobio,date_prelevement,code_fraction, resultat) %>% 
  filter(code_parametre==1433) %>% 
  arrange(code_station_hydrobio)


verif_fraction_po4 <- verif_fraction_po4 %>%
  group_by(date_prelevement, code_station_hydrobio) %>%
  mutate(
    score = case_when(
      n_distinct(code_fraction) > 1 ~ 3,
      code_fraction == 3 ~ 1,
      code_fraction == 23 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


ggplot(verif_fraction_po4,
       aes(x = as.factor(date_prelevement), 
           y = factor(code_station_hydrobio),
           fill = as.factor(score))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "red","3"="violet")) +
  labs(
    title = "Méthode d'analyse orthophosphates",
    x = "Date",
    y = "Station",
    fill = "Score"
  ) +
  theme_classic()

#Pour le phosphore total
verif_fraction_poT <- parametres_physico %>%
  select(code_parametre,code_station_hydrobio,date_prelevement,code_fraction, resultat) %>% 
  filter(code_parametre==1350) %>% 
  arrange(code_station_hydrobio)


verif_fraction_poT <- verif_fraction_poT %>%
  group_by(date_prelevement, code_station_hydrobio) %>%
  mutate(
    score = case_when(
      n_distinct(code_fraction) > 1 ~ 3,
      code_fraction == 3 ~ 1,
      code_fraction == 23 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


ggplot(verif_fraction_poT,
       aes(x = as.factor(date_prelevement), 
           y = factor(code_station_hydrobio),
           fill = as.factor(score))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "red","3"="violet")) +
  labs(
    title = "Méthode d'analyse Phosphore total",
    x = "Date",
    y = "Station",
    fill = "Score"
  ) +
  theme_classic()

#Pour MES
verif_fraction_mes <- parametres_physico %>%
  select(code_parametre,code_station_hydrobio,date_prelevement,code_fraction, resultat) %>% 
  filter(code_parametre==1305) %>% 
  arrange(code_station_hydrobio)


verif_fraction_mes <- verif_fraction_mes %>%
  group_by(date_prelevement, code_station_hydrobio) %>%
  mutate(
    score = case_when(
      n_distinct(code_fraction) > 1 ~ 3,
      code_fraction == 3 ~ 1,
      code_fraction == 23 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


ggplot(verif_fraction_mes,
       aes(x = as.factor(date_prelevement), 
           y = factor(code_station_hydrobio),
           fill = as.factor(score))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "blue", "2" = "red","3"="violet")) +
  labs(
    title = "Méthode d'analyse MES",
    x = "Date",
    y = "Station",
    fill = "Score"
  ) +
  theme_classic() 
  

#Pour conductivé
verif_fraction_cond <- parametres_physico %>%
  select(code_parametre,code_station_hydrobio,date_prelevement,code_fraction, resultat) %>% 
  filter(code_parametre==1313) %>% 
  arrange(code_station_hydrobio)


verif_fraction_cond <- verif_fraction_cond %>%
  group_by(date_prelevement, code_station_hydrobio) %>%
  mutate(
    score = case_when(
      n_distinct(code_fraction) > 1 ~ 3,
      code_fraction == 3 ~ 1,
      code_fraction == 23 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


ggplot(verif_fraction_cond,
       aes(x = as.factor(date_prelevement), 
           y = factor(code_station_hydrobio),
           fill = as.factor(score))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "blue", "2" = "red","3"="violet")) +
  labs(
    title = "Méthode d'analyse conductivité",
    x = "Date",
    y = "Station",
    fill = "Score"
  ) +
  theme_classic() 

# Carbone organique 

verif_fraction_orga <- parametres_physico %>%
  select(code_parametre,code_station_hydrobio,date_prelevement,code_fraction, resultat) %>% 
  filter(code_parametre==1841) %>% 
  arrange(code_station_hydrobio)


verif_fraction_orga <- verif_fraction_orga %>%
  group_by(date_prelevement, code_station_hydrobio) %>%
  mutate(
    score = case_when(
      n_distinct(code_fraction) > 1 ~ 3,
      code_fraction == 3 ~ 1,
      code_fraction == 23 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


ggplot(verif_fraction_orga,
       aes(x = as.factor(date_prelevement), 
           y = factor(code_station_hydrobio),
           fill = as.factor(score))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "blue", "2" = "red","3"="violet")) +
  labs(
    title = "Méthode d'analyse Carbone organique",
    x = "Date",
    y = "Station",
    fill = "Score"
  ) +
  theme_classic() 

