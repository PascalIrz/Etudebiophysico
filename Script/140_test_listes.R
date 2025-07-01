library(vegan)
library(ape)

taxo_minv_wide <- abondance_par_station_annee %>%
  group_by(code_station_hydrobio, code_appel_taxon) %>%
  summarise(abondance = sum(abondance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = code_appel_taxon, values_from = abondance, values_fill = 0) %>% 
  arrange(code_station_hydrobio)

taxo_minv_wide <- as.data.frame(lapply(mat, as.numeric))

# Matrice d’abondance uniquement (retirer la colonne station)
mat <- as.data.frame(taxo_minv_wide[,-1])
row.names(mat) <- taxo_minv_wide$code_station_hydrobio
mat <- as.data.frame(lapply(mat, as.numeric))

# Transformer les valeurs ?


# Faire NMDS
nmds <- metaMDS(taxo_minv_wide, distance = "bray", k = 2, trymax = 100)
plot(nmds, type = "t")


#Extraire les résultats

#Trouver les centreoids

#Faire le graphique du NMDS

# Dissimilarité/ Matrice des distances (Bray-Curtis)
dist_mat <- vegdist(mat, method = "bray")  # ou method = "jaccard"
as.matrix(dist_mat)
plot(hclust(dist_mat), main = "Clustering hiérarchique")

# Hypothèses 


# Permanova
test <- adonis2(mat ~ code_station_hydrobio, data = taxo_minv_wide, method = "bray")

beta <- betadiver(mat, method = "sor")  # Indice de Sørensen par exemple
plot(hclust(beta))


# TEST

#CCA
ac_liste <- cca(taxo_minv_wide)
summary(ac_liste, display = "reg") 
plot(ac_liste, scaling = "sites")
scores(ac_liste)


#

pcoa_liste <- pcoa(dist_mat, correction = "lingoes")
pcoa_liste$values
biplot(pcoa_liste)
biplot(pcoa_liste,taxo_minv_wide)
