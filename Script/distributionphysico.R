
ggplot(parametres_physico, aes(x =resultat)) +
  geom_histogram(bins = 100, fill = "#0072B2", color = "white", alpha = 0.7) +  # Histogramme avec 30 bins
  facet_wrap(~ libelle_parametre, scales = "free") +
  labs(title = "Distribution des valeurs",
       x = "",
       y = "Fréquence") +
  theme_minimal()


parametres_physico_mois <- parametres_physico %>% 
  group_by(code_parametre,mois) %>% 
  summarise(
    moy_mois=mean(resultat, na.rm =TRUE),
    med_mois=median(resultat, na.rm = TRUE),
    ecart_type_mois=sd(resultat, na.rm = TRUE),
    max_mois=max(resultat, na.rm =TRUE),
    min_mois=min(resultat, na.rm = TRUE)
  )

ggplot(parametres_physico, aes (x=factor(mois), y = resultat)) +
  geom_boxplot() +
  facet_wrap(~ code_parametre, scales = "free_y") +
  labs(x = "Mois", y= "Valeur", title ="Distribution des parametres par mois") +
  theme_bw()


