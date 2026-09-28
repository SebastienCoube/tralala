


sampleLatentState = function(model, transition_params, initial_state = NULL){
  # looping over individuals
  for(ind_idx in seq(length(model$data_list))){
    # initializing vector
    model$data_list[[ind_idx]]$latent_states <- rep(NA, length(model$data_list[[ind_idx]]$emissions))
    # initial state
    if(!is.null(initial_state)){
      if(!initial_state %in% model$transition_model$latent_states){stop("initial_state must be one of the latent states")}
      model$data_list[[ind_idx]]$latent_states[1] <- initial_state
      }else{model$data_list[[ind_idx]]$latent_states[1] <- sample(model$transition_model$latent_states, size = 1)}
    model$data_list[[ind_idx]]$time_spent <- rep(NA, length(model$data_list[[ind_idx]]$emissions))
    # initial time spent
    if(is.null(model$transition_model$BTF_per_state[[model$data_list[[ind_idx]]$latent_states[[1]]]])){
      model$data_list[[ind_idx]]$time_spent[1] <- 1}
    if(!is.null(model$transition_model$BTF_per_state[[model$data_list[[ind_idx]]$latent_states[[1]]]])){
      model$data_list[[ind_idx]]$time_spent[1] <-
        ceiling(max(model$transition_model$BTF_per_state[[model$data_list[[ind_idx]]$latent_states[[1]]]]) *runif(1))
    }
    # looping over time
    for(time_idx in seq(2, length(model$data_list[[ind_idx]]$emissions))){
      # getting transition probability
      current_latent_state <- model$data_list[[ind_idx]]$latent_states[time_idx-1]
      current_time_spent <- model$data_list[[ind_idx]]$time_spent[time_idx-1]
      current_BTF <- model$transition_model$BTF_per_state[[current_latent_state]]
      if(model$transition_model$misc$is_state_absorbing[[current_latent_state]]){
        model$data_list[[ind_idx]]$latent_states[time_idx] <- model$data_list[[ind_idx]]$latent_states[time_idx-1]
      }else{
        transition_probs <- value_at_btf_knots(
          explanatory_variable = model$data_list[[ind_idx]]$explanatory_variables[time_idx,],
          transition_param =  transition_params[[current_latent_state]]
        )
        if(current_time_spent>=model$transition_model$misc$depth[current_latent_state]){
          transition_probs <- transition_probs[nrow(transition_probs),]
        }else{
          prev_btf_idx  <- max(which(current_BTF<=current_time_spent))
          BTF_mix_1 <- (current_time_spent-current_BTF[prev_btf_idx])/model$transition_model$misc$BTFstepsize[[current_latent_state]][prev_btf_idx]
          BTF_mix_2 <- (current_BTF[prev_btf_idx+1]-current_time_spent)/model$transition_model$misc$BTFstepsize[[current_latent_state]][prev_btf_idx]
          transition_probs <-
            transition_probs[prev_btf_idx,]^BTF_mix_1 *
            transition_probs[prev_btf_idx+1,]^BTF_mix_2
          transition_probs <- transition_probs/sum(transition_probs)
        }
        # sampling new latent state
        model$data_list[[ind_idx]]$latent_states[time_idx] <-
          sample(
            c(model$data_list[[ind_idx]]$latent_states[time_idx-1], names(transition_probs)[-1]),
            1,F, transition_probs)
      }
      # updating time spent
      model$data_list[[ind_idx]]$time_spent[time_idx] <- 1
      if(model$data_list[[ind_idx]]$latent_states[time_idx-1] == model$data_list[[ind_idx]]$latent_states[time_idx]){
        model$data_list[[ind_idx]]$time_spent[time_idx] <- model$data_list[[ind_idx]]$time_spent[time_idx-1]+1
      }
    }
  }
  return(model)
}



sampleEmissions = function(model, emission_regression_coefficients, sample_one_emission){
  for(ind_idx in seq(length(model$data_list))){
    for(time_idx in seq(1, length(model$data_list[[ind_idx]]$emissions))){
      model$data_list[[ind_idx]]$emissions[[time_idx]] <- sample_one_emission(
        emission_regression_coefficients,
        explanatory_variable = model$data_list[[ind_idx]]$explanatory_variables[time_idx,],
        latent_state = model$data_list[[ind_idx]]$latent_states[time_idx]
      )
    }
  }
  return(model)
}
