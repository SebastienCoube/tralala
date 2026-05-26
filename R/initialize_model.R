initializeModelStep1 = function(
    latent_states_names,
    data_list,
    fixed_emission_param_names,
    estimated_emission_param_names,
    emission_log_likelihood,
    emission_log_prior
){
  res = list()
  # check data
  checkDataList(data_list)
  # transition model
  res$transition_model = beginTransitionModel(
    explanatory_variable_names = colnames(data_list[[1]]$explanatory_variables_transition),
    latent_states_names = latent_states_names)
  # emission model
  res$emission_model = beginEmissionModel(
    nvar = ncol(data_list[[1]]$explanatory_variables_emission),
    latent_states_names = latent_states_names,
    fixed_emission_param_names = fixed_emission_param_names,
    estimated_emission_param_names = estimated_emission_param_names,
    emission_log_likelihood = emission_log_likelihood,
    emission_log_prior = emission_log_prior
  )
  res$data_list = data_list
  res
}

initializeModelStep2 = function(model){
  checkFixedEmissionParams(
    latent_states_names = model$transition_model$dont_touch$latent_states_names,
    fixed_emission_params_names = model$emission_model$dont_touch$fixed_emission_param_names,
    fixed_emission_params = model$emission_model$to_specify$fixed_emission_params)
  model$emission_model = c(model$emission_model$dont_touch, model$emission_model$to_specify)
  checkTransitionMat(model$transition_model)
  checkBTF(model$transition_model)
  model$transition_model = createExplanatoryVariablesEffects(model$transition_model)
  return(model)
}

initializeModelStep3 = function(model){
  checkExplanatoryVariablesEffects(model$transition_model)
  model$transition_model = c(model$transition_model$dont_touch, model$transition_model$to_specify)
  model$transition_model$outstates =
    apply(model$transition_model$possible_transitions, 1, function(x)model$transition_model$latent_states_names[which(x)])
  model$transition_model$outstates = mapply(
    setdiff, model$transition_model$outstates, model$transition_model$latent_states_names
  )
  model$transition_model$number_outstates = lapply(model$transition_model$outstates, length)
  return(model)
}

# Creates transition parameters with the right dimensions
createParams = function(model){
  transition_model_params = lapply(
    model$transition_model$latent_states_names, function(latent_state_name){
      if(is.null(model$transition_model$explanatory_variables_effects[[latent_state_name]]))return(NULL)

      res = apply(
        model$transition_model$explanatory_variables_effects[[latent_state_name]], 1,
        function(x){
          if(x[1])return(NULL)
          if(x[2]){
            res = matrix(0, 1, model$transition_model$number_outstates[[latent_state_name]])
            colnames(res) = model$transition_model$outstates[[latent_state_name]]
            return(res)
            }
          if(x[3]){
            res = matrix(
              0, 1 + length(model$transition_model$BTF_per_state[[latent_state_name]]),
              model$transition_model$number_outstates[[latent_state_name]])
              colnames(res) = model$transition_model$outstates[[latent_state_name]]
            return(res)
            }
        },
        simplify = F)
    })
  names(transition_model_params) = model$transition_model$latent_states_names
  emission_params = matrix(0, length(model$emission_model$estimated_emission_param_names), length(model$transition_model$latent_states_names))
  colnames(emission_params) = model$transition_model$latent_states_names
  row.names(emission_params) = model$emission_model$estimated_emission_param_names
  return(list("transition_params" = transition_model_params, "emission_params" = emission_params))
}





