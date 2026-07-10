

#' emission_log_likelihood is a function that takes as arguments:
#' - estimated_emission_param_vec, a real-valued vector of emission parameters who are estimated by the model
#' - fixed_emission_param_vec,     a real-valued vector of emission parameters who are fixed by the user
#' - emission, an observation from the emissions slot of an element from data_list
#' - explanatory_variable_vec, a real-valued vector taken as a row of explanatory_variables_emission from an element from data_list
#' Important note: the function should be evaluable and twice differentiable for any value of estimated_emission_param_vec.
#' Be careful with parameters such as a standard deviation, a scale, and whatnot, who are positive. Those must be passed to the log !
#' estimated_emission_param_vec and fixed_emission_param_vec are the parameters associated with one label of the latent state

#' emission_log_prior is a function that takes as arguments:
#' - estimated_emission_param_vec, a real-valued vector of emission parameters who are estimated by the model
#' Important note: the function should be evaluable and twice differentiable for any value of estimated_emission_param_vec
#' Be careful with parameters such as a standard deviation, a scale, and whatnot, who are positive. Those must be passed to the log !
#' Also be careful with hard constraints such as "this parameter must be greater than that parameter".
#' This breaks the differentiability requirement, and can also lead to monstruous posteriors (see Robert, Marin, Mergensen)
#' Last important note : to ensure the posterior being valid, the prior must be a valid distribution as well.


#'emission_log_likelihood =      function(estimated_emission_param_vec, fixed_emission_param_vec, emission, explanatory_variable_vec){
#'}
#'emission_log_likelihood_grad = function(estimated_emission_param_vec, fixed_emission_param_vec, emission, explanatory_variable_vec){
#'}
#'emission_log_prior = function(estimated_emission_param_mat){
#'}

checkEmissionLogLikelihood1 = function(f){
  if(!is.function(f))stop(paste("emission_log_likelihood must be a function"))
  if(!("emission" %in% formalArgs(f))){
    stop("The emission log-likelihood must have `emission` among its arguments")
  }
  if(length(formalArgs(f))<2)stop("The emission log-likelihood must have have at least two arguments: `emission` and one or more emission parameters ")
  return(invisible())
}

createExplanatoryVariablesEffectsEmission = function(model){
  model$emission_model$dont_touch = c(
    model$emission_model$dont_touch,
    model$emission_model$to_specify
  )


  model$emission_model$to_specify$explanatory_variables_effects <- matrix(
    T, length(model$emission_model$dont_touch$emission_param_names),
    length(model$transition_model$explanatory_variable_names))
  row.names(model$emission_model$to_specify$explanatory_variables_effects) <-
    model$emission_model$dont_touch$emission_param_names
  colnames(model$emission_model$to_specify$explanatory_variables_effects) <-
    model$transition_model$explanatory_variable_names
  return(model)
}

