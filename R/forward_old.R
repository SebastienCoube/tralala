tbfMatch = function(tbf, tbf_ref)


preprocessTbf_ = function(transition_model, params){
  if(any(!transition_model$to_specify$possible_transitions %in% c(0, 1)))stop("transition_model$to_specify$possible_transitions must be filled with 0's or 1's only")
  if(any(!na.omit(transition_model$to_specify$variable_basis_interactions) %in% names(transition_model$dont_touch$temporal_basis_function_list) ))stop("transition_model$to_specify$variable_basis_interactions must be filled with NA's or temporal basis function names (from transition_model$dont_touch$temporal_basis_function_list)")
  # pre-multiply TBFs by respective parameters

  # exit states
  n_exit_states = apply(transition_model$to_specify$possible_transitions, 1, function(x)sum(x))
  is_state_absorbing = (n_exit_states<2)
  exit_states = lapply(
    transition_model$dont_touch$latent_states_names,
    function(x)setdiff(
      transition_model$dont_touch$latent_states_names[which(transition_model$to_specify$possible_transitions[x,]!=0)],
      x))
  names(exit_states) = transition_model$dont_touch$latent_states_names
  # semi-Markov depth
  tbf_used_per_state = apply(transition_model$to_specify$variable_basis_interactions, 1, function(x)as.vector(na.omit(unique(x))), simplify = F)
  semi_markov_depths_per_tbf = sapply(transition_model$dont_touch$temporal_basis_function_list, function(x)max(x))
  semi_markov_depths_per_state = sapply(
    transition_model$dont_touch$latent_states_names,
    function(x){
      if(is_state_absorbing[x])return(1)
      return(max(sapply(tbf_used_per_state[x], function(x)max(transition_model$dont_touch$temporal_basis_function_list[[(x)]]))))
    }
  )
  # pre-multiplying transition parameters by covariates
  pre_multiplied_params = list()
  for(origin_state in transition_model$dont_touch$latent_states_names){
    params$transition_params
  }
  active_explanatory_variables = apply(
    transition_model$to_specify$variable_basis_interactions, 1,
    function(x)colnames(transition_model$to_specify$variable_basis_interactions)[which(!is.na(x))])
  return(list(
    softmax_matrices = softmax_matrices,
    pre_multiplied_tbfs = pre_multiplied_tbfs,
    semi_markov_depths_per_state = semi_markov_depths_per_state,
    exit_states = exit_states,
    is_state_absorbing = is_state_absorbing,
    active_explanatory_variables = active_explanatory_variables))
}



forward = function(data_seq, transition_model, emission_model, params, storealpha = F){
  if(any(is.na(emission_model$to_specify$fixed_emission_params)))stop("There ane NAs in emission_model$to_specify$fixed_emission_params, please fill by hand")
  preprocessed_tbf = preprocessTbf(transition_model = transition_model, params = params)
  n_time_periods = nrow(data_seq$explanatory_variables_transition)
  # initializing log(p(data, latent state, time counter | parameters))
  alpha = lapply(
    preprocessed_tbf$semi_markov_depths_per_state,
    function(x){
      matrix(-log(x)-log(length(preprocessed_tbf$semi_markov_depths_per_state)), 1, x)
    }
  )
  if(storealpha){
    store_alpha = lapply(alpha, function(x)matrix(0, length(x), length(data_seq$emissions)))
  }

  # checking sum to 1
  # sapply(lapply(alpha, exp), sum)
  # sum(sapply(lapply(alpha, exp), sum))

  # probabilities to enter in a new state
  new_state_log_probabilities = rep(-Inf, length(transition_model$dont_touch$latent_states_names))
  names(new_state_log_probabilities) = transition_model$dont_touch$latent_states_names
  for(time_idx in seq(length(data_seq$emissions))){
    # transition
    if(time_idx>1){
      new_state_log_probabilities[] = -Inf
      for(s in model$transition_model$dont_touch$latent_states_names[which(!preprocessed_tbf$is_state_absorbing)] ){
        # computing log transition probabilities
        for(s_ in preprocessed_tbf$exit_states[[s]]){
          preprocessed_tbf$softmax_matrices[[s]][,s_] =
            preprocessed_tbf$pre_multiplied_tbfs[[s]][[s_]] %*% data_seq$explanatory_variables_transition[time_idx-1, preprocessed_tbf$active_explanatory_variables[[s]]]
        }
        # softmax renormalization
        preprocessed_tbf$softmax_matrices[[s]][,ncol(preprocessed_tbf$softmax_matrices[[s]])] =
          apply(
            preprocessed_tbf$softmax_matrices[[s]][, - ncol(preprocessed_tbf$softmax_matrices[[s]]), drop=F],
            1,
            function(x) matrixStats::logSumExp(c(x, 0))
          )
        preprocessed_tbf$softmax_matrices[[s]][, - ncol(preprocessed_tbf$softmax_matrices[[s]])] =
          preprocessed_tbf$softmax_matrices[[s]][, - ncol(preprocessed_tbf$softmax_matrices[[s]])] -
          preprocessed_tbf$softmax_matrices[[s]][,ncol(preprocessed_tbf$softmax_matrices[[s]])]
        # checking that prob vectors sum to 1
        #print(
        # apply(
        #   exp(preprocessed_tbf$softmax_matrices[[s]][, - ncol(preprocessed_tbf$softmax_matrices[[s]])]), 1, sum
        #   ) +
        #   exp(- preprocessed_tbf$softmax_matrices[[s]][, ncol(preprocessed_tbf$softmax_matrices[[s]])])
        #)
        # computing log-probabilities of exit from a state and incrementing to probabilities of reaching a new state
        for(s_ in preprocessed_tbf$exit_states[[s]]){
          new_state_log_probabilities[s_]  =
            matrixStats::logSumExp(c(
                new_state_log_probabilities[s_],
                preprocessed_tbf$softmax_matrices[[s]][,s_] + alpha[[s]]
              ))
        }
        # computing probabilities to remain in the same state, with increase of counter
        # alpha at semi-Markov depth
        alpha[[s]][length(alpha[[s]])] = alpha[[s]][length(alpha[[s]])] - preprocessed_tbf$softmax_matrices[[s]][length(alpha[[s]]),ncol(preprocessed_tbf$softmax_matrices[[s]])]
        if(length(alpha[[s]])>1){
          # tranferring from penultimate to last
          alpha[[s]][length(alpha[[s]])] = matrixStats::logSumExp(c(
            alpha[[s]][length(alpha[[s]])],
            alpha[[s]][length(alpha[[s]])-1] - preprocessed_tbf$softmax_matrices[[s]][length(alpha[[s]])-1,ncol(preprocessed_tbf$softmax_matrices[[s]])]
          ))
          # alpha before semi-Markov depth being shifted along time counter
          if(length(alpha[[s]])>2){
            alpha[[s]][seq(2, length(alpha[[s]])-1)] =
              alpha[[s]][seq(1, length(alpha[[s]])-2)] - preprocessed_tbf$softmax_matrices[[s]][seq(1, length(alpha[[s]])-2),ncol(preprocessed_tbf$softmax_matrices[[s]])]
          }
          # resetting first alpha
          alpha[[s]][1] = -Inf
        }
      }
      # Probabilitites with time counter equal to 1
      for(s in model$transition_model$dont_touch$latent_states_names[which(!preprocessed_tbf$is_state_absorbing)] ){
        alpha[[s]][1] = matrixStats::logSumExp(c(
          alpha[[s]][1],
          new_state_log_probabilities[s]
        ))
      }
    }
    # re-weighting by emission density
    for(s in transition_model$dont_touch$latent_states_names){
      alpha[[s]] = alpha[[s]] +
        emission_model$dont_touch$log_likelihood(
          estimated_emission_param_vec = params$emission_params[,s],
          fixed_emission_param_vec = emission_model$to_specify$fixed_emission_params[,s],
          emission = data_seq$emissions[[time_idx]])

      if(storealpha)store_alpha[[s]][,time_idx] = alpha[[s]]
    }
  }
  res = list("alpha" = alpha)
  if(storealpha)res$storealpha = store_alpha
  res
}
