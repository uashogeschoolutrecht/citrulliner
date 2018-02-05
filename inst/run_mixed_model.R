#' Run a multilevel model
#'
#' @param 
#' @param 
#' @return 
#' 
#'
#'
#'
run_mixed_model <- function(dependent_variable,
                            fixed_effect_variable,
                            random_effect_variable,
                            grouping_variable_random_intercept){
  
  model <- lmer(dependent_variable ~ fixed_effect_variable + 
                  (random_effect_variable | grouping_variable_random_intercept))
    
  return(model)
  
  
}