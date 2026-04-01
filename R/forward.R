tbfMatch = function(tbf, tbf_ref)

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
      lapply(transition_model$dont_touch$latent_states_names[!is_state_absorbing], function(state_name){
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
    names(single_to_combined_conversion) = transition_model$dont_touch$latent_states_names[!is_state_absorbing]
    active_explanatory_variables_per_state = apply(
      transition_model$to_specify$variable_basis_interactions, 1,
      function(x)colnames(transition_model$to_specify$variable_basis_interactions)[which(!is.na(x))])
    pre_multiplied_combined_tbf = lapply(transition_model$dont_touch$latent_states_names[!is_state_absorbing], function(state_name){
      sapply(active_explanatory_variables_per_state[[state_name]], function(explanatory_variable_name){
        tbf_name = model$transition_model$to_specify$variable_basis_interactions[state_name, explanatory_variable_name]
        #print(tbf_name)
        conversion_mat = single_to_combined_conversion[[state_name]][[tbf_name]]
        transition_param_value = params$transition_params[[state_name]][[explanatory_variable_name]]
        res = conversion_mat %*% transition_param_value
      })
    })
    names(pre_multiplied_combined_tbf) = transition_model$dont_touch$latent_states_names[!is_state_absorbing]

    return(list(
      semi_markov_depths_per_state = semi_markov_depths_per_state,
      exit_states = exit_states,
      n_exit_states = n_exit_states,
      exit_states_mat = exit_states_mat,
      is_state_absorbing = is_state_absorbing,
      semi_markov_depths_per_state = semi_markov_depths_per_state,
      active_explanatory_variables_per_state = active_explanatory_variables_per_state,
      combined_tbf_per_state = combined_tbf_per_state,
      single_to_combined_conversion = single_to_combined_conversion))
  }


preprocessed_tbf = preprocessTbf_(transition_model = model$transition_model, params = params)

forward_ = function(preprocessed_tbf, data_seq, model, params, storealpha = F, grad = F){

  logalpha_dimnames = c("val")
  if(grad){
    logalpha_dimnames = c(logalpha_dimnames, outer(row.names(params$emission_params), colnames(params$emission_params), function(x, y)(paste("Emission", x, y, sep=  "_"))))
    tr_params_grad = lapply(params$transition_params, function(x)lapply(x, function(y)outer(colnames(y), seq(nrow(y)), paste, sep = "_")))
    tr_params_grad = lapply(tr_params_grad, function(x)mapply(paste, names(x), x, sep = "_"))
    tr_params_grad = mapply(paste, names(tr_params_grad), tr_params_grad, sep = "_")
  }
  logalpha = array(0, c(max(preprocessed_tbf$semi_markov_depths_per_state), 1, model$transition_model$dont_touch$n_latent_states))

  dimnames(logalpha) = list(NULL, NULL, model$transition_model$dont_touch$latent_states_names)

  for(i in seq(length(data_seq$emissions))){
    print(i)
  }
  preprocessed_tbf
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
