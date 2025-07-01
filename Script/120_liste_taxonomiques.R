library(hubeau)
# library(dplyr)
# library(stringr) #pour str_extract
library(ggplot2)
library(tidyverse)
library(tibble)
library(readxl)
library(sf)
library(DT)
library(httr)#utiliser POST pour calcul i2m2 à partir du SEEE
library(trend)
library(ggrepel)
library(mapview)
library(lubridate)
library(httr)
library(openxlsx)
library(stringr)
library(vegan)

load(file = "Data/10_donnees_pretraitees.rda")

functions <- list.files(path = "R",
                        pattern = ".R$",
                        full.names = TRUE)

map(.x = functions,
    .f = source)

###############################################################################
# MINV
###############################################################################

#On sélectionne seulement les stations étudiées dans la partie I2M2
station_minv <- unique(clean_minv$code_station_hydrobio)

#On charge la liste
Liste_taxo_minv <- map_df(station_minv,f_get_liste_taxo_minv)

# On ajoute la colonne année
Liste_taxo_minv <-Liste_taxo_minv %>% 
  mutate(annee = year(date_prelevement))

Liste_taxo_minv <- Liste_taxo_minv %>% 
  select(c(code_station_hydrobio, libelle_station_hydrobio, date_prelevement, code_appel_taxon, libelle_appel_taxon, annee, longitude, latitude))

save(Liste_taxo_minv,file="Data/ListeTaxonomique_new.Rdata")
load(file="Data/ListeTaxonomique_new.Rdata")


# On charge le fichier excel pour avoir les ordres

ordre <- read_excel("C:/Users/ilona.garcia/Documents/RstudioGIT/Etudebiophysico/Data/BD_MINV_2023FM.xlsx")
ordre <- rename(ordre, "code_appel_taxon"="CODE_TAXON") 
ordre$code_appel_taxon <- as.character(ordre$code_appel_taxon)

liste_taxon_complete <- Liste_taxo_minv %>% 
  left_join(ordre, by = "code_appel_taxon") %>% 
  select(c(-FAMILLE_A, -GENRE_B, -NIVEAU, -NUM_REF))


# On retire les années inférieures à 2012 car changement de protocole
liste_taxon_complete_new <- liste_taxon_complete %>% 
  filter(annee >=2013)

# Ajout du document labo

ajout_new <- read_excel("C:/Users/ilona.garcia/Documents/RstudioGIT/Etudebiophysico/Data/TableMINV2025_Ilona_new.xlsx")

liste_taxon_ajout <- liste_taxon_complete_new %>% 
  left_join(ajout_new, by = "libelle_appel_taxon") %>% 
  drop_na()

liste_taxon_ajout <- liste_taxon_ajout %>% 
  select(-GROUPE,-code_appel_taxon)

liste_taxon_ajout <- liste_taxon_ajout %>% 
  rename("code_appel_taxon" = "Cd_Taxon_norm")

liste_taxon_ajout <- liste_taxon_ajout %>%
  mutate(GroupTaxo = recode(GroupTaxo,
                            "01-PLECOPTERA" = "Plecoptera",
                            "02-TRICHOPTERA" = "Trichoptera",
                            "03-EPHEMEROPTERA" = "Ephemeroptera",
                            "04-HETEROPTERA" = "Heteroptera",
                            "05-COLEOPTERA" = "Coleoptera",
                            "06-DIPTERA" = "Diptera",
                            "07-ODONATA" = "Odonata",
                            "08-MEGALOPTERA" = "Megaloptera",
                            "09-PLANIPENNIA" = "Planipennia",
                            "10-HYMENOPTERA" = "Hymenoptera",
                            "11-LEPIDOPTERA" = "Lepidoptera",
                            "12-CRUSTACEA" = "Crustacea",
                            "13-CRUSTACEA-BRANCHIOPODA-PHYLLOPODA" = "Crustacea_branchiopoda_phyllopoda",
                            "14-CRUSTACEA (Autres)" = "Crustacea_autres",
                            "15-HYDRACARINA" = "Hydracarina",
                            "16-BIVALVIA" = "Bivalvia",
                            "17-GASTROPODA" = "Gastropoda",
                            "18-BRANCHIOBDELLIDA" = "Branchiobdellida",
                            "19-HIRUDINEA" = "Hirudinea",
                            "20-OLIGOCHAETA" = "Oligochaeta",
                            "21-POLYCHAETA" = "Polychaeta",
                            "22-TURBELLARIA" = "Turbellaria",
                            "23-NEMERTEA" = "Nemertea",
                            "24-NEMATHELMINTHA" = "Nemathelmintha",
                            "25-HYDROZOA" = "Hydrozoa",
                            "26-PORIFERA / SPONGIAIRES" = "Porifera_spongiaires",
                            "27-BRYOZOA" = "Bryozoa",
                            "40-ARTHROPODA" = "Arthropoda",
                            "50-ANIMALIA" = "Animalia",
                            "90-Taxon vivant en milieu marin" = "Taxon_marin",
                            "M" = "M",
                            "M+HM" = "M_HM"
  ))


save(liste_taxon_complete, file = "Data/liste_taxo_minv_complete.Rdata")

save(liste_taxon_complete_new, file = "Data/liste_taxo_minv_complete_new.Rdata")
load(file ="Data/liste_taxo_minv_complete_new.Rdata")

save(liste_taxon_ajout, file = "Data/liste_taxon_ajout.Rdata")
load(file ="Data/liste_taxon_ajout.Rdata")

# Connaitre la liste des taxons
liste_taxons <- liste_taxon_ajout %>% 
  distinct(libelle_appel_taxon) %>% 
  arrange(libelle_appel_taxon)

#Nombre d'ordre
liste_ordre <- liste_taxon_ajout %>% 
  distinct(GroupTaxo) %>% 
  arrange(GroupTaxo)


# On calcule l'abondance de chaque taxon par station et année
abondance_par_station_annee <- liste_taxon_ajout %>% 
  group_by(code_station_hydrobio,libelle_station_hydrobio, annee, code_appel_taxon, GroupTaxo, longitude, latitude) %>% 
  summarise(abondance = n(), .groups = "drop")


#On calcule l'abondance relative 

abondance_relative <- abondance_par_station_annee %>% 
  group_by(code_station_hydrobio, annee) %>% 
  mutate(total_abondance = sum(abondance, na.rm = TRUE),
  abondance_rel = abondance / total_abondance) %>% 
  ungroup()

abondance_relative <- abondance_relative %>% 
  mutate(pourcentage = 100 * abondance / total_abondance)

ggplot(abondance_relative, aes(x = annee, y = total_abondance, color = code_station_hydrobio,
                          group = code_station_hydrobio)) + geom_line() + geom_point() +
  labs(title = "Evolution de l'abondance totale par station",
       x = "Année",
       y = "Abondance totale",
       color = "station") +
  theme_minimal()

abondance_relative %>%
  distinct(code_station_hydrobio, annee, total_abondance) %>%
  group_by(annee) %>%
  summarise(abondance_moy = mean(total_abondance, na.rm = TRUE),
            nb_stations = n(),
            .groups = "drop"
            ) %>%
  ggplot(aes(x = annee, y = abondance_moy)) +
  geom_line(color = "#D55E00") +
  geom_point(color = "#D55E00") +
  geom_text(aes(label = nb_stations), vjust = -1.2, size = 3) +
  labs(
    title = "Évolution annuelle de l'abondance moyenne",
    subtitle = "Nombre de stations indiqué au-dessus des points",
    x = "Année", y = "Abondance moyenne"
  ) +
  theme_minimal()


#Les stations qui varient peu et celles qui varient beaucoup

variation_stations <- abondance_relative %>%
  group_by(code_station_hydrobio) %>%
  summarise(
    moy_abondance = mean(total_abondance, na.rm = TRUE),
    sd_abondance = sd(total_abondance, na.rm = TRUE),
    cv_abondance = sd_abondance / moy_abondance  # Coefficient de variation
  ) %>%
  arrange(desc(cv_abondance)) %>% 
  mutate(libelle_station_hydrobio = clean_minv$libelle_station_hydrobio[match(code_station_hydrobio, clean_minv$code_station_hydrobio)])
 

stations_stables <- variation_stations %>%
  filter(cv_abondance < 0.5) 

stations_instables <- variation_stations %>%
  filter(cv_abondance >= 0.5 & cv_abondance <=1)


# On calcule la richesse spécifique pour chaque station et année 
richesse_taxo <- liste_taxon_ajout %>% 
  group_by(code_station_hydrobio, annee, longitude, latitude) %>%
  summarise(nb_taxons = n_distinct(code_appel_taxon), .groups = "drop")

#Calcule de la richesse par ordre

richesse_taxo_ordre <- liste_taxon_ajout %>% 
  group_by(code_station_hydrobio, annee, GroupTaxo) %>% 
  summarise(richesse_ordre = n_distinct(code_appel_taxon), .groups = "drop") %>% 
  ungroup()

# Contribution relative des ordres à la richesse 
 contribution_ordre <- richesse_taxo_ordre %>% 
   left_join(richesse_taxo, by = c("code_station_hydrobio", "annee")) %>% 
   mutate(pourcentage = 100 * richesse_ordre /nb_taxons)
 
# Contribution moyenne par ordre

ordre_contribution_moyenne <- contribution_ordre %>% 
  group_by(GroupTaxo) %>% 
  summarise(contribution_moyenne = mean(pourcentage, na.rm = TRUE),
            nombre_occurences = n()) %>% 
  arrange(desc(contribution_moyenne))


# Graphique contribution moyenne des ordres

ggplot(ordre_contribution_moyenne, aes(x = reorder(GroupTaxo, -contribution_moyenne), y = contribution_moyenne)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Contribution moyenne des ordres à la richesse",
       x = "Ordre", y = "Contribution moyenne (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#On veut voir l'évolution de la richesse au cours du temps dans chaque station
ggplot(richesse_taxo, aes(x = annee, y = nb_taxons, color = code_station_hydrobio,
                          group = code_station_hydrobio)) + geom_line() + geom_point() +
  labs(title = "Evolution de la richesse par station",
       x = "Année",
       y = "richesse",
       color = "station") +
  theme_minimal()

variation_stations_richesse <- richesse_taxo %>%
  group_by(code_station_hydrobio) %>%
  summarise(
    moy_richesse = mean(nb_taxons, na.rm = TRUE),
    sd_richesse = sd(nb_taxons, na.rm = TRUE),
    cv_richesse = sd_richesse / moy_richesse  # Coefficient de variation
  ) %>%
  arrange(desc(cv_richesse)) %>% 
  mutate(libelle_station_hydrobio = clean_minv$libelle_station_hydrobio[match(code_station_hydrobio, clean_minv$code_station_hydrobio)])


stations_stables_richesse <- variation_stations_richesse %>%
  filter(cv_richesse < 0.5) 

stations_instables_richesse <- variation_stations_richesse %>%
  filter(cv_richesse >= 1.5)

# Station avec - de 40 taxons 

station_40 <- richesse_taxo %>% 
  filter(nb_taxons <= 40) %>% 
  distinct(code_station_hydrobio)


# On regarde quels taxons apparaissent qu'une seule fois
rare_taxons <- liste_taxon_ajout %>% 
  group_by(code_station_hydrobio, code_appel_taxon) %>% 
  summarise(nb_occurences = n(), .groups = "drop") %>% 
  filter(nb_occurences == 1)

# ON calcule le pourcentage d'EPT par an et par station

## On compte les individus EPT

ept_par_station_annee <- abondance_relative %>%
  filter(GroupTaxo %in% c("Ephemeroptera", "Plecoptera", "Trichoptera")) %>%
  group_by(code_station_hydrobio, annee) %>%
  summarise(abondance_EPT = sum(abondance, na.rm = TRUE), .groups = "drop")

abondance_EPT <- abondance_relative %>%
  left_join(ept_par_station_annee, by = c("code_station_hydrobio", "annee")) %>%
  mutate(pourcentage_EPT = 100 * abondance_EPT / total_abondance)

abondance_EPT %>%
  distinct(code_station_hydrobio, annee, pourcentage_EPT) %>%
  group_by(annee) %>%
  summarise(pourcentage_EPT_moy = mean(pourcentage_EPT, na.rm = TRUE)) %>%
  ggplot(aes(x = annee, y = pourcentage_EPT_moy)) +
  geom_line(color = "#D55E00") +
  geom_point(color = "#D55E00") +
  labs(
    title = "Évolution annuelle du % EPT moyen",
    x = "Année", y = "Pourcentage EPT moyen"
  ) +
  theme_minimal()

# Pourcentage Chironome 

chiro_par_station_annee <- abondance_relative %>%
  filter(code_appel_taxon == "807") %>%
  group_by(code_station_hydrobio, annee) %>%
  summarise(abondance_chiro = sum(abondance, na.rm = TRUE), .groups = "drop")

abondance_chiro <- abondance_relative %>%
  left_join(chiro_par_station_annee, by = c("code_station_hydrobio", "annee")) %>%
  mutate(pourcentage_chiro = 100 * abondance_chiro / total_abondance)

abondance_chiro %>%
  distinct(code_station_hydrobio, annee, pourcentage_chiro) %>%
  group_by(annee) %>%
  summarise(pourcentage_chiro_moy = mean(pourcentage_chiro, na.rm = TRUE)) %>%
  ggplot(aes(x = annee, y = pourcentage_chiro_moy)) +
  geom_line(color = "#D55E00") +
  geom_point(color = "#D55E00") +
  labs(
    title = "Évolution annuelle du % Chironomes moyen",
    x = "Année", y = "Pourcentage Chironomes moyen"
  ) +
  theme_minimal()

# Rapport EPT/Chironnome 

ratio_ept_chiro <- abondance_relative %>%
  mutate(groupe = case_when(
    GroupTaxo %in% c("Ephemeroptera", "Plecoptera", "Trichoptera") ~ "EPT",
    code_appel_taxon == "807" ~ "Chironomidae" ,
    TRUE ~ "autre"
  )) %>%
  filter(groupe %in% c("EPT", "Chironomidae")) %>%
  group_by(code_station_hydrobio, annee, groupe) %>%
  summarise(abondance = sum(abondance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = groupe, values_from = abondance, values_fill = 0) %>%
  mutate(indice_EPT_chiro = (EPT / (EPT + Chironomidae)) * 100)

ratio_ept_chiro %>%
  distinct(code_station_hydrobio, annee, indice_EPT_chiro) %>%
  group_by(annee) %>%
  summarise(moyenne_rapport = mean(indice_EPT_chiro, na.rm = TRUE)) %>%
  ggplot(aes(x = annee, y = moyenne_rapport)) +
  geom_line(color = "#D55E00") +
  geom_point(color = "#D55E00") +
  labs(title = "Évolution annuelle du rapport EPT/Chironomes",
       x = "Année", y = "Rapport EPT/chironome moyen") +
  theme_minimal()

# On regarde les Gammares 

gam_par_station_annee <- abondance_relative %>%
  filter(code_appel_taxon %in% c("892", "888","887")) %>%
  group_by(code_station_hydrobio, annee) %>%
  summarise(abondance_gam = sum(abondance, na.rm = TRUE), .groups = "drop")

abondance_gam <- abondance_relative %>%
  left_join(gam_par_station_annee, by = c("code_station_hydrobio", "annee")) %>%
  mutate(pourcentage_gam = 100 * abondance_gam / total_abondance)

abondance_gam %>%
  distinct(code_station_hydrobio, annee, pourcentage_gam) %>%
  group_by(annee) %>%
  summarise(pourcentage_gam_moy = mean(pourcentage_gam, na.rm = TRUE)) %>%
  ggplot(aes(x = annee, y = pourcentage_gam_moy)) +
  geom_line(color = "#D55E00") +
  geom_point(color = "#D55E00") +
  labs(
    title = "Évolution annuelle du % Gammare moyen",
    x = "Année", y = "Pourcentage Gammare moyen"
  ) +
  theme_minimal()

# Diptères 

dipt_par_station_annee <- abondance_relative %>%
  filter(GroupTaxo == "Diptera") %>%
  group_by(code_station_hydrobio, annee) %>%
  summarise(abondance_dipt = sum(abondance, na.rm = TRUE), .groups = "drop")

abondance_dipt <- abondance_relative %>%
  left_join(dipt_par_station_annee, by = c("code_station_hydrobio", "annee")) %>%
  mutate(pourcentage_dipt = 100 * abondance_dipt / total_abondance)

abondance_dipt %>%
  distinct(code_station_hydrobio, annee, pourcentage_dipt) %>%
  group_by(annee) %>%
  summarise(pourcentage_dipt_moy = mean(pourcentage_dipt, na.rm = TRUE)) %>%
  ggplot(aes(x = annee, y = pourcentage_dipt_moy)) +
  geom_line(color = "#D55E00") +
  geom_point(color = "#D55E00") +
  labs(
    title = "Évolution annuelle du % Diptères moyen",
    x = "Année", y = "Pourcentage Diptère moyen"
  ) +
  theme_minimal()

# On vérifie s'il y a des stations où il n'y a pas d'EPT 
ept_stations <- liste_taxon_ajout %>% 
  filter(GroupTaxo %in% c("Ephemeroptera", "Plecoptera", "Trichoptera")) %>% 
  distinct(code_station_hydrobio)

toutes_stations <- liste_taxon_ajout %>% 
  distinct(code_station_hydrobio)

stations_sans_ept <- anti_join(toutes_stations, ept_stations, by = "code_station_hydrobio")

# Indice de shannon et simpson


matrix_abondance <- abondance_par_station_annee %>% 
  pivot_wider(names_from = code_appel_taxon, values_from = abondance, values_fill = 0)

mat_abond_valeurs <- matrix_abondance %>% select(-code_station_hydrobio,-libelle_station_hydrobio,-GroupTaxo, -longitude, -latitude, -annee)

indices_div <- matrix_abondance %>%
  select(code_station_hydrobio, annee) %>%
  mutate(
    shannon = diversity(mat_abond_valeurs, index = "shannon"),
    simpson = diversity(mat_abond_valeurs, index = "simpson")
  )

indices_div %>%
  group_by(annee) %>%
  summarise(shannon_moy = mean(shannon), simpson_moy = mean(simpson)) %>%
  ggplot(aes(x = annee)) +
  geom_line(aes(y = shannon_moy), color = "blue") +
  geom_line(aes(y = simpson_moy), color = "green") +
  labs(y = "Indice moyen", title = "Évolution des indices de diversité")

save(abondance_relative,
     richesse_taxo,
     file = "Data/df_taxo.rda"
  
)

################################################################################
# DIAT
################################################################################

stations_ibd <- unique(clean_ibd$code_station_hydrobio)
  

if (file.exists("Data/ListeTaxonomique_diat.Rdata"))
{
  
  load("Data/ListeTaxonomique_diat.Rdata")
} else
{
  liste_taxo_diat <- map_df(stations_ibd,f_get_liste_taxo_diat) #%>%
    
    save(liste_taxo_diat,file="Data/ListeTaxonomique_diat.Rdata")
}

liste_taxo_diat <- liste_taxo_diat %>%
  mutate(annee=year(date_prelevement))

abondance_par_station_annee_diat <- liste_taxo_diat %>% 
  group_by(code_station_hydrobio, annee, code_appel_taxon) %>% 
  summarise(abondance = n(), .groups = "drop")

abondance_relative_diat <- abondance_par_station_annee_diat %>% 
  group_by(code_station_hydrobio, annee) %>% 
  mutate(total_abondance = sum(abondance, na.rm = TRUE),
         abondance_rel = abondance / total_abondance) %>% 
  ungroup()

abondance_relative_diat <- abondance_relative_diat %>% 
  mutate(pourcentage = 100 * abondance / total_abondance)


abondance_relative_diat %>%
  distinct(code_station_hydrobio, annee, total_abondance) %>%
  group_by(annee) %>%
  summarise(abondance_moy = mean(total_abondance, na.rm = TRUE)) %>%
  ggplot(aes(x = annee, y = abondance_moy)) +
  geom_line(color = "#D55E00") +
  geom_point(color = "#D55E00") +
  labs(
    title = "Évolution annuelle de l'abondance moyenne",
    x = "Année", y = "Abondance moyenne"
  ) +
  theme_minimal()


richesse_taxo_diat_filtre <- abondance_relative_diat %>% 
  filter(pourcentage >= 5)

liste <- richesse_taxo_diat_filtre %>% 
  distinct(code_appel_taxon)

richesse_taxo_diat_calcul <- richesse_taxo_diat_filtre %>% 
  group_by(code_station_hydrobio, annee) %>% 
  summarise(nb_taxons = n_distinct(code_appel_taxon), .groups = "drop")

richesse_taxo_diat_calcul %>%
  distinct(code_station_hydrobio, annee, nb_taxons) %>%
  group_by(annee) %>%
  summarise(richesse_moy = mean(nb_taxons, na.rm = TRUE)) %>%
  ggplot(aes(x = annee, y = richesse_moy)) +
  geom_line(color = "#D55E00") +
  geom_point(color = "#D55E00") +
  labs(
    title = "Évolution annuelle de la richesse moyenne",
    x = "Année", y = "Abondance moyenne"
  ) +
  theme_minimal()


save(abondance_relative, file = "Data/120_donnees_pretraitees_liste.rda")





#tableau de contingence : AFC selon taille du BV ? Selon station seulement ? 
#Traits écologiques 

