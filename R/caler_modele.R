#' Résume les modèles par métrique en fonction des prédicteurs
#' GLM gaussien, lien identité, pas de transformation de variable.
#'
#' @param df Dataframe contenant les données.
#' @param var_dependante Texte. Nom de la métrique à modéliser (var dépendante)
#'
#' @return Dataframe avec les résultats
#' @export
#'
#' @examples
caler_modele <- function(df, var_dependante = "I2M2") {
  
  my_formula <- as.formula(
    paste0(var_dependante,
           "~ O2dissous + NH4 + NO3 + Ptot"))
  
  mod <- glm(
    formula = my_formula,
    data = df,
    family = gaussian(link = "identity")
  )
  # summary(mod)
  mod_simpl <- MASS::stepAIC(mod)
  
  coef <- mod_simpl$coefficients
  sig <- coef(summary(mod_simpl))[,4]
  
  res <- data.frame(coef, sig) %>% 
    mutate(sig2 = case_when(
      sig <= 0.001 ~ "***",
      sig > 0.001 & sig < 0.01 ~ "**",
      sig > 0.01 & sig < 0.05 ~ "*",
      sig > 0.05 & sig < 0.1 ~ ".",
      sig > 0.1 ~ "ns",
      TRUE ~ NA
    ),
    texte = paste0(
      round(coef, 2),
      "(",
      sig2,
      ")"),
    texte = ifelse(sig2 == "ns", "", texte)) %>% 
    rownames_to_column(var = "variable") %>% 
    dplyr::select(variable, coef = texte) %>% 
    mutate(metrique = var_dependante)

}
