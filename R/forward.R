
  # x = tent_matrix(knots = c(1, seq(3)*10), eval_points = seq(30))
  # plot(x[,1])
  # plot(x[,2])
  # plot(x[,3])
  # plot(x[,4])
tent_matrix <- function(knots, eval_points) {
  knots_sorted <- sort(unique(knots))
  all_knots    <- c(0L, knots_sorted)

  n_knots <- length(knots_sorted)
  n_eval  <- length(eval_points)

  mat <- matrix(0.0, nrow = n_eval, ncol = n_knots)
  rownames(mat) <- eval_points
  colnames(mat) <- knots_sorted

  for (i in seq_len(n_knots)) {
    k      <- all_knots[i + 1]
    k_prev <- all_knots[i]
    k_next <- all_knots[i + 2]

    for (j in seq_len(n_eval)) {
      x <- eval_points[j]

      if (x > k_prev && x < k) {
        mat[j, i] <- (x - k_prev) / (k - k_prev)
      } else if (x == k) {
        mat[j, i] <- 1.0
      } else if (!is.na(k_next) && x > k && x < k_next) {
        mat[j, i] <- (k_next - x) / (k_next - k)
      }
    }
  }

  mat
}



  preprocessTbf_ = function(transition_model, params){
    if(any(!transition_model$to_specify$possible_transitions %in% c(0, 1)))stop("transition_model$to_specify$possible_transitions must be filled with 0's or 1's only")
    if(any(!na.omit(transition_model$to_specify$variable_basis_interactions) %in% names(transition_model$dont_touch$temporal_basis_function_list) ))stop("transition_model$to_specify$variable_basis_interactions must be filled with NA's or temporal basis function names (from transition_model$dont_touch$temporal_basis_function_list)")
    # pre-multiply TBFs by respective parameters

    # exit states
    n_exit_states = apply(transition_model$to_specify$possible_transitions, 1, function(x)sum(x)-1)
    is_state_absorbing = (n_exit_states<2)
    non_absorbing_states = transition_model$dont_touch$latent_states_names[!is_state_absorbing]
    exit_states = lapply(
      transition_model$dont_touch$latent_states_names,
      function(x)setdiff(
        transition_model$dont_touch$latent_states_names[which(transition_model$to_specify$possible_transitions[x,]!=0)],
        x))
    names(exit_states) = model$transition_model$dont_touch$latent_states_names
    exit_states_mat = cbind(
      "origin" = unlist(mapply(function(x, y)rep(x, y),
      seq(model$transition_model$dont_touch$n_latent_states),
      n_exit_states
      )),
      "exit" = unlist(sapply(exit_states, function(x)match(x, transition_model$dont_touch$latent_states_names)))
    )
    row.names(exit_states_mat) = NULL
    # semi-Markov depth
    tbf_used_per_state = apply(transition_model$to_specify$variable_basis_interactions, 1, function(x)as.vector(na.omit(unique(x))), simplify = F)
    semi_markov_depths_per_tbf = sapply(transition_model$dont_touch$temporal_basis_function_list, function(x)max(x))
    semi_markov_depths_per_state = sapply(
      transition_model$dont_touch$latent_states_names,
      function(states_name){
        if(is_state_absorbing[states_name])return(1)
        return(max(sapply(tbf_used_per_state[[states_name]], function(x)max(transition_model$dont_touch$temporal_basis_function_list[[(x)]]))))
      }
    )
    # combining temporal basis functions of one state
    combined_tbf_per_state = lapply(tbf_used_per_state[which(!is_state_absorbing)], function(tbf_names){
      if(length(tbf_names)>0){
      sort(unique(as.vector(
        sapply(tbf_names, function(tbf_name)transition_model$dont_touch$temporal_basis_function_list[[tbf_name]])[]
      )))
      }
    })
    single_to_combined_conversion =
      lapply(non_absorbing_states, function(state_name){
        combined_tbf = combined_tbf_per_state[[state_name]]
        out =
          lapply(tbf_used_per_state[[state_name]], function(tbf_name){
            if(identical(combined_tbf, 1))return(matrix(1,1))
            tbf = model$transition_model$dont_touch$temporal_basis_function_list[[tbf_name]]
            return(tent_matrix(knots = tbf, eval_points = combined_tbf))
          })
        names(out) = tbf_used_per_state[[state_name]]
        out
      })
    names(single_to_combined_conversion) = non_absorbing_states
    active_explanatory_variables_per_state = apply(
      transition_model$to_specify$variable_basis_interactions, 1,
      function(x)colnames(transition_model$to_specify$variable_basis_interactions)[which(!is.na(x))])

    pre_multiplied_combined_tbf = lapply(non_absorbing_states, function(state_name){
      res = array(
        0,
        dim =  c(
          length(combined_tbf_per_state[[state_name]]),
          length(active_explanatory_variables_per_state[[state_name]]),
          n_exit_states[state_name]
        ))
      dimnames(res) = list(
          combined_tbf_per_state[[state_name]],
          active_explanatory_variables_per_state[[state_name]],
          exit_states[[state_name]]
        )
      res
    })
    names(pre_multiplied_combined_tbf) = non_absorbing_states
    for(state_name in non_absorbing_states){
      for(explanatory_variable in active_explanatory_variables_per_state[[state_name]]){
        pre_multiplied_combined_tbf[[state_name]][,explanatory_variable,] <-
          single_to_combined_conversion[[state_name]][[transition_model$to_specify$variable_basis_interactions[state_name, explanatory_variable]]] %*%
          params$transition_params[[state_name]][[explanatory_variable]]
      }
    }


    return(
      list(
        semi_markov_depths_per_state = semi_markov_depths_per_state,
        exit_states = exit_states,
        n_exit_states = n_exit_states,
        exit_states_mat = exit_states_mat,
        is_state_absorbing = is_state_absorbing,
        non_absorbing_states = non_absorbing_states,
        active_explanatory_variables_per_state = active_explanatory_variables_per_state,
        combined_tbf_per_state = combined_tbf_per_state,
        single_to_combined_conversion = single_to_combined_conversion,
        pre_multiplied_combined_tbf = pre_multiplied_combined_tbf)
    )
  }



softmax = function(x){
  res = c(1, exp(x))
  res = res/sum(res)
  res
}
normalize = function(x){x / sum(x)}


preprocessed_tbf = preprocessTbf_(transition_model = model$transition_model, params = params)

forward_ = function(preprocessed_tbf, data_seq, model, params, storealpha = F, grad = F){

  # filtering probabilities
  logalpha = lapply(
    model$transition_model$dont_touch$latent_states_names,
    function(state_name)unname(rep(log(1/(model$transition_model$dont_touch$n_latent_states * preprocessed_tbf$semi_markov_depths_per_state[state_name])), preprocessed_tbf$semi_markov_depths_per_state[state_name])))
  names(logalpha) = model$transition_model$dont_touch$latent_states_names
  # sum(exp(unlist(logalpha)))

  for(i in seq(2, length(data_seq$emissions))){
    # transition ####
    probs_from_exit = rep(0, model$transition_model$dont_touch$n_latent_states); names(probs_from_exit) = model$transition_model$dont_touch$latent_states_names
    for(state_name in preprocessed_tbf$non_absorbing_states){
      depth = preprocessed_tbf$semi_markov_depths_per_state[[state_name]]
      outstates = preprocessed_tbf$exit_states[[state_name]]
      value_at_node = matrix(
        apply(
        preprocessed_tbf$pre_multiplied_combined_tbf[[state_name]], c(3),
        function(slice) {
          slice %*% data_seq$explanatory_variables_transition[i-1,preprocessed_tbf$active_explanatory_variables_per_state[[state_name]]]
          }),
        ncol  = preprocessed_tbf$n_exit_states[[state_name]])
      row.names(value_at_node) = dimnames(preprocessed_tbf$pre_multiplied_combined_tbf[[state_name]])[[1]]
      colnames(value_at_node) = outstates
      # updating filtering probabilities at semi-Markov depth
      #probs_from_exit[outstates] = probs_from_exit[outstates] + exp(logalpha[[state_name]][depth]) * tonic[-1]
      #logalpha[[state_name]][depth] = logalpha[[state_name]][depth] + log(tonic[1])
      # if depth > 0, make probability trickle down
      tonic = softmax(value_at_node[nrow(value_at_node),])
      next_node_idx = nrow(value_at_node)
      next_tonic = tonic

      for(u in seq(depth, 1)){
        probs_from_exit[outstates] = probs_from_exit[outstates] + exp(logalpha[[state_name]][depth]) * tonic[-1]
        logalpha[[state_name]][depth] = logalpha[[state_name]][depth] + log(tonic[1])

        if(u>1 & u == preprocessed_tbf$combined_tbf_per_state[[state_name]][next_node_idx]){
          next_node_idx = next_node_idx - 1
          tonic = next_tonic
          next_tonic = softmax(value_at_node[next_node_idx,])
          delta_tonic = (next_tonic / tonic)^(1 / )
          print(" ")
        }

        print(paste("u = ", u, " next_node_idx = ", next_node_idx, " next node = ", preprocessed_tbf$combined_tbf_per_state[[state_name]][next_node_idx]))
        print(" ")
        tonic = normalize(tonic * delta_tonic)
      }
    }
    # updating filtering probabilities at depth = 1
    for(state_name in model$transition_model$dont_touch$latent_states_names){
      logalpha[state_name] = logalpha[state_name] + log(probs_from_exit[state_name])
    }

    # emission ####
    ###############
    for(state_name in transition_model$dont_touch$latent_states_names){
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
