beginModel = function(
    latent_states_names,
    data_list,
    fixed_emission_param_names,
    estimated_emission_param_names,
    emission_log_likelihood,  emission_log_likelihood_grad = NULL,
    emission_log_prior

){
  res = list()
  # check data
  checkDataList(data_list)
  # transition model
  res$transition_model = beginTransitionModel(
    data_list = data_list,
    latent_states_names = latent_states_names)
  # emission model
  res$emission_model = beginEmissionModel(
    data_list,
    latent_states_names = latent_states_names,
    fixed_emission_param_names = fixed_emission_param_names,
    estimated_emission_param_names = estimated_emission_param_names,
    emission_log_likelihood = emission_log_likelihood,  emission_log_likelihood_grad = emission_log_likelihood_grad,
    emission_log_prior = emission_log_prior
  )
  res$data_list = data_list
  res
}




# Creates transition parameters with the right dimensions
createParams = function(model){

  transition_model_params = lapply(
    model$transition_model$dont_touch$latent_states_names, function(latent_state_name){
      lapply(model$transition_model$to_specify$variable_basis_interactions[latent_state_name,],
             function(BTF_name){
               if(is.na(BTF_name))return(NULL)
               res = matrix(0,
                            nrow = length(model$transition_model$dont_touch$temporal_basis_function_list[[BTF_name]]+1),
                            ncol = sum(model$transition_model$to_specify$possible_transitions[latent_state_name,])-1)
               colnames(res) = model$transition_model$dont_touch$latent_states_names[
                 setdiff(which(model$transition_model$to_specify$possible_transitions[latent_state_name,]!=0), match(latent_state_name, model$transition_model$dont_touch$latent_states_names))
               ]
               return(res)
             })
    })
  names(transition_model_params) =  model$transition_model$dont_touch$latent_states_names
  for(i in seq(length(transition_model_params), 1)){
    for(j in seq(length(transition_model_params[[i]]), 1)){
      if(is.null(transition_model_params[[i]][[j]]))transition_model_params[[i]][[j]] = NULL
    }
  }
  for(i in seq(length(transition_model_params), 1)){
    if(length(transition_model_params[[i]])==0)transition_model_params[[i]] = NULL
  }
  emission_params = matrix(0, length(model$emission_model$dont_touch$estimated_emission_param_names), length(model$transition_model$dont_touch$latent_states_names))
  colnames(emission_params) = model$transition_model$dont_touch$latent_states_names
  row.names(emission_params) = model$emission_model$dont_touch$estimated_emission_param_names
  return(list("transition_params" = transition_model_params, "emission_params" = emission_params))
}

plotTransitionGraph = function(model){
  g = model$transition_model$to_specify$possible_transitions
  diag(g) = 0
  g = igraph::graph_from_adjacency_matrix(g)
  plot(g)
}



