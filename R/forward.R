


preprocessTbf = function(params, model){
  res = list()
  for(origin_state in names(params$transition_params)){
    res[[origin_state]] = list()
    if(!model$transition_model$misc$is_state_absorbing[[origin_state]]){
      res[[origin_state]] = list()
      for(i in seq(model$transition_model$misc$BTFdim[origin_state]+1)){
        res[[origin_state]][[i]] =
          do.call(cbind, lapply(params$transition_params[[origin_state]], function(x)x[min(i, nrow(x)),]))
      }
      #for(outstate in colnames(params$transition_params[[origin_state]][[1]])){
      #  res[[origin_state]][[outstate]] =
      #    matrix(0, 1 + model$transition_model$misc$BTFdim[origin_state], length(model$transition_model$misc$active_explanatory_variables[[origin_state]]))
      #  colnames(res[[origin_state]][[outstate]]) = model$transition_model$misc$active_explanatory_variables[[origin_state]]
      #  for(explanatory_variable in model$transition_model$misc$active_explanatory_variables[[origin_state]]){
      #    res[[origin_state]][[outstate]][,explanatory_variable] =
      #      params$transition_params[[origin_state]][[explanatory_variable]][,outstate]
      #  }
      #}
    }
  }
  res
}



forward_ = function(preprocessed_tbf, data_seq, model, params, storealpha = F, grad = F){

  # filtering probabilities
  logalpha = lapply(model$transition_model$misc$depth, function(x)rep(log(1/(model$transition_model$n_latent_states * x)), x))
  # sum(exp(unlist(logalpha)))


  for(i in seq(2, length(data_seq$emissions))){
    # transition ####
    probs_from_exit = rep(0, model$transition_model$n_latent_states); names(probs_from_exit) = model$transition_model$latent_states
    for(state_name in model$transition_model$misc$non_absorbing_states){
      depth = model$transition_model$misc$depth[state_name]
      outstates = model$transition_model$misc$outstates[[state_name]]
      explanatory_variables = model$transition_model$misc$active_explanatory_variables[[state_name]]

      # updating filtering probabilities at semi-Markov depth
      BTF_idx = model$transition_model$misc$BTFdim[[state_name]]+1
      tonic = softmax(preprocessed_tbf[[state_name]][[BTF_idx]] %*% data_seq$explanatory_variables_transition[i-1,explanatory_variables] )
      next_tonic = tonic

      for(u in seq(depth, 1)){
        probs_from_exit[outstates] = probs_from_exit[outstates] + exp(logalpha[[state_name]][depth]) * tonic[-1]
        logalpha[[state_name]][depth] = logalpha[[state_name]][depth] + log(tonic[1])

        if(u>1 & u == preprocessed_tbf$combined_tbf_per_state[[state_name]][next_node_idx]){
          next_node_idx = next_node_idx - 1
          tonic = next_tonic
          next_tonic = softmax(value_at_node[next_node_idx,])
          #delta_tonic = (next_tonic / tonic)^(1 / )
          print(" ")
        }

        print(paste("u = ", u, " next_node_idx = ", next_node_idx, " next node = ", preprocessed_tbf$combined_tbf_per_state[[state_name]][next_node_idx]))
        print(" ")
        tonic = normalize(tonic * delta_tonic)
      }
    }
    # updating filtering probabilities at depth = 1
    for(state_name in model$transition_model$dont_touch$latent_states){
      logalpha[state_name] = logalpha[state_name] + log(probs_from_exit[state_name])
    }

    # emission ####
    ###############
    for(state_name in transition_model$dont_touch$latent_states){
    }
  }
}


forward = function(data_seq, transition_model, emission_model, params, storealpha = F){
  if(any(is.na(emission_model$to_specify$fixed_emission_params)))stop("There ane NAs in emission_model$to_specify$fixed_emission_params, please fill by hand")
  preprocessed_tbf = preprocessTbf_(transition_model = transition_model, params = params)
  n_time_periods = nrow(data_seq$explanatory_variables_transition)
  # initializing log(p(data, latent state, time counter | parameters))
  alpha = matrix(
    0,
    transition_model$dont_touch$n_latent_states,
    max(preprocessed_tbf$semi_markov_depths_per_state),
  )

  if(storealpha){
    store_alpha = lapply(alpha, function(x)matrix(0, length(x), length(data_seq$emissions)))
  }

  # checking sum to 1
  # sapply(lapply(alpha, exp), sum)
  # sum(sapply(lapply(alpha, exp), sum))

  # probabilities to enter in a new state
  new_state_log_probabilities = rep(-Inf, length(transition_model$dont_touch$latent_states))
  names(new_state_log_probabilities) = transition_model$dont_touch$latent_states
  for(time_idx in seq(length(data_seq$emissions))){
    # transition
    if(time_idx>1){
      new_state_log_probabilities[] = -Inf
      for(s in model$transition_model$dont_touch$latent_states[which(!preprocessed_tbf$is_state_absorbing)] ){
        # computing log transition probabilities
        for(s_ in preprocessed_tbf$exit_states[[s]]){
          preprocessed_tbf$softmax_matrices[[s]][,s_] =
            preprocessed_tbf$pre_multiplied_tbfs[[s]][[s_]] %*% data_seq$explanatory_variables_transition[time_idx-1, preprocessed_tbf$active_explanatory_variables_per_state[[s]]]
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
      for(s in model$transition_model$dont_touch$latent_states[which(!preprocessed_tbf$is_state_absorbing)] ){
        alpha[[s]][1] = matrixStats::logSumExp(c(
          alpha[[s]][1],
          new_state_log_probabilities[s]
        ))
      }
    }
    # re-weighting by emission density
    for(s in transition_model$dont_touch$latent_states){
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
