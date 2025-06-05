#' Calage d'un modèle de métrique en fonction de prédicteurs
#' GLM gaussien, lien identité, pas de transformation de variable.
#'
#' @param df Dataframe contenant les données.
#' @param var_dependante Texte. Nom de la métrique à modéliser (var dépendante)
#'
#' @return Affichage des résultats et graphiques de diag
#' @export
#'
#' @examples
analyser_modele <- function(df, var_dependante = "I2M2") {
  
  my_formula <- as.formula(
    paste0(var_dependante,
           "~ O2dissous + NH4 + NO3 + Ptot"))
  
  mod <- glm(
    formula = my_formula,
    data = df,
    family = gaussian(link = "identity")
  )
  
  mod_simpl <- MASS::stepAIC(mod)

  bptest(mod_simpl, ~fitted(mod_simpl) + I(fitted(mod_simpl)^2)) # test de white, variance constante

  # plot(fitted(mod_simpl),
  #      residuals(mod_simpl,type="pearson"),
  #      xlab="valeurs prédites", ylab="résidus de pearson",
  #      main="résidus de pearson vs valeurs prédites")
  # 
   vif(mod_simpl)
  # 
  # qqnorm(residuals(mod_simpl)) #normalité respectée
  # qqline(residuals(mod_simpl))
  # hist(residuals(mod_simpl), breaks=30,
  #      main = "Distribution",
  #      xlab="Résidus")
  # shapiro.test(residuals(mod_simpl)) #si pvalue < 0,05 ce n'est pas normal
  
  plot(mod_simpl)
}
