


#' plot(piecewiseLinear(c(1, 10, 100, 1000), c(100, 1000, 100, 1000), 2000))
#' plot(piecewiseLinear(c(1, 10, 100, 1000), c(100, 1000, 100, 1000), 2000), log = "x")

piecewiseLinear = function(tbf, param, depth){
  if(length(tbf)==1) return(rep(param, depth))
  res = rep(0, depth)
  for(i in seq(length(tbf))){
    res[seq(tbf[i], c(tbf[-1], depth)[i])] = seq(param[i], param[min(i+1, length(tbf))], length.out = c(tbf[-1], depth)[i] - tbf[i] + 1)
  }
  res
}


preprocessTbf = function(transition_model, params){
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
  # pre-allocating Sofmtax matrices
  softmax_matrices = lapply(
    transition_model$dont_touch$latent_states_names,
    function(x)
    {
      if(is_state_absorbing[[x]])return(NULL)
      res = matrix(0, semi_markov_depths_per_state[x], n_exit_states)
      colnames(res) = c(
        exit_states[[x]],
        "sum")
      return(res)
    })
  names(softmax_matrices) = transition_model$dont_touch$latent_states_names
  # pre-multiplying TBFs by parameters
  pre_multiplied_tbfs = list()
  for(origin_state in transition_model$dont_touch$latent_states_names){
    pre_multiplied_tbfs[[origin_state]] = list()
    if(!is_state_absorbing[origin_state]){
      for(destination_state in exit_states[[origin_state]]){
        idx = !is.na(transition_model$to_specify$variable_basis_interactions[origin_state,])
        pre_multiplied_tbfs[[origin_state]][[destination_state]] =
          matrix(
            0,
            semi_markov_depths_per_state[[origin_state]],
            sum(idx)
          )
        colnames(pre_multiplied_tbfs[[origin_state]][[destination_state]]) =
          transition_model$dont_touch$explanatory_variable_names[which(idx)]
        for(varname in colnames(pre_multiplied_tbfs[[origin_state]][[destination_state]])){
          pre_multiplied_tbfs[[origin_state]][[destination_state]][,varname] =
            piecewiseLinear(
              tbf = transition_model$dont_touch$temporal_basis_function_list[[transition_model$to_specify$variable_basis_interactions[origin_state,varname]]],
              param = params$transition_params[[origin_state]][[varname]][,destination_state],
              depth = semi_markov_depths_per_state[origin_state]
            )
        }
      }
    }
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

sampleLatentState = function(n_samples, initial_state = NULL, initial_time_counter = NULL,  model, params,  data_seq){
  if((!is.null(initial_state)) | (!is.null(initial_time_counter))){
    if(length(initial_state)!= length(initial_time_counter))stop("initial_state and initial_time_counter must have the same length")
    if(any(is.na(match(initial_state, model$transition_model$dont_touch$latent_states_names))))stop(paste("initial_state must be a vector with elements", model$transition_model$dont_touch$latent_states_names))
    if(any(as.integer(initial_time_counter) - initial_time_counter != 0) | any(initial_time_counter < 0))stop(paste("initial_time_counter must be a vector of positive integers"))
    n_samples = length(initial_state)
  }

  preprocessed_tbf = preprocessTbf(transition_model = model$transition_model, params = params)
  n_time_periods = nrow(data_seq$explanatory_variables_transition)
  # drawing initial states
  latent_state =
    matrix("0", n_time_periods, n_samples)
  if(is.null(initial_state)) initial_state = model$transition_model$dont_touch$latent_states_names[cut(runif(n_samples), c(0, seq(model$transition_model$dont_touch$n_latent_states)/model$transition_model$dont_touch$n_latent_states))]
  latent_state[1,] = initial_state
  time_counter =
    matrix(0L, n_time_periods, n_samples)
  if(is.null(initial_time_counter)){
    initial_time_counter = sapply(
      latent_state[1,],
      function(state) ceiling(runif(1)*preprocessed_tbf$semi_markov_depths_per_state[state])
    )
  }
  time_counter[1,] = initial_time_counter
  # sampling states
  for(time_idx in seq(2, n_time_periods)){
    # softmax
    for(s in model$transition_model$dont_touch$latent_states_names[which(!preprocessed_tbf$is_state_absorbing)] ){
      preprocessed_tbf$softmax_matrices[[s]][,ncol(preprocessed_tbf$softmax_matrices[[s]])] = 1
      for(s_ in preprocessed_tbf$exit_states[[s]]){
        preprocessed_tbf$softmax_matrices[[s]][,s_] =
          exp(preprocessed_tbf$pre_multiplied_tbfs[[s]][[s_]] %*% data_seq$explanatory_variables_transition[time_idx, preprocessed_tbf$active_explanatory_variables[[s]]])
        preprocessed_tbf$softmax_matrices[[s]][,ncol(preprocessed_tbf$softmax_matrices[[s]])] =
          preprocessed_tbf$softmax_matrices[[s]][,ncol(preprocessed_tbf$softmax_matrices[[s]])] +
          preprocessed_tbf$softmax_matrices[[s]][,s_]
        preprocessed_tbf$softmax_matrices[[s]][,-ncol(preprocessed_tbf$softmax_matrices[[s]])] =
          preprocessed_tbf$softmax_matrices[[s]][,-ncol(preprocessed_tbf$softmax_matrices[[s]])] /
          preprocessed_tbf$softmax_matrices[[s]][,ncol(preprocessed_tbf$softmax_matrices[[s]])]
      }
    }
    for(s in model$transition_model$dont_touch$latent_states_names[which(preprocessed_tbf$is_state_absorbing)] ){
      preprocessed_tbf$softmax_matrices[[s]] = matrix(1,1,1,dimnames = list(NULL, s))
    }

    for(sample_idx in seq(n_samples)){
      current_state = latent_state[time_idx-1, sample_idx]
      current_counter = time_counter[time_idx-1, sample_idx]
      probvec = preprocessed_tbf$softmax_matrices[[current_state]][
        min(preprocessed_tbf$semi_markov_depths_per_state[[current_state]], current_counter),
        -length(preprocessed_tbf$softmax_matrices[[current_state]])]
      latent_state[time_idx, sample_idx] =
        c(preprocessed_tbf$exit_states[[current_state]], current_state)[1 + sum(runif(1)>cumsum(probvec))]
      if(latent_state[time_idx, sample_idx] == latent_state[time_idx-1, sample_idx]){
        time_counter[time_idx, sample_idx] = 1 + time_counter[time_idx-1, sample_idx]
      }else{time_counter[time_idx, sample_idx] = 1}
    }
  }
  return(list(latent_state = latent_state, time_counter = time_counter))
}



sampleObservations = function(data_seq, model, params, latent_state_seq, emission_sampler){
  argnames = c("estimated_emission_params_vec", "fixed_emission_params_vec", "explanatory_variables_emission")
  if(any(is.na(match(formalArgs(emission_sampler), argnames))))stop(paste(
    "The formal arguments of the function emission_sampler must be",
    Reduce(paste, argnames)
  ))
  res = list()
  for(i in seq(length(latent_state_seq))){
    if(is.null(data_seq["explanatory_variables_emission"]))  {
      res[[i]] = emission_sampler(
        estimated_emission_params_vec = params$emission_params[,latent_state_seq[i]],
        fixed_emission_params_vec = model$emission_model$to_specify$fixed_emission_params[,latent_state_seq[i]]
      )
    }
    if(!is.null(data_seq["explanatory_variables_emission"]))  {
      res[[i]] = emission_sampler(
        explanatory_variables_emission = data_seq["explanatory_variables_emission"][i,],
        estimated_emission_params_vec = params$emission_params[,latent_state_seq[i]],
        fixed_emission_params_vec = model$emission_model$to_specify$fixed_emission_params[,latent_state_seq[i]]
      )
    }
  }
  return(res)
}


plotLatentState = function(latent_state_sequence, model){
  plot(1,1, type = "n",
       xlim = c(-length(latent_state_sequence)/10, length(latent_state_sequence)), ylim = c(1, .1+model$transition_model$dont_touch$n_latent_states),
       xlab = "time", ylab = "latent state")
  points(match(latent_state_sequence, model$transition_model$dont_touch$latent_states_names), type=  "b")
  for(i in seq(length(model$transition_model$dont_touch$latent_states_names))){
    text(-length(latent_state_sequence)/10, i+.1, model$transition_model$dont_touch$latent_states_names[i])
    abline(h = i)
  }
}

forward = function(data_seq, transition_model, emission_model, params){
  if(any(is.na(emission_model$to_specify$fixed_emission_params)))stop("There ane NAs in emission_model$to_specify$fixed_emission_params, please fill by hand")
  preprocessed_tbf = preprocessTbf(transition_model = transition_model, params = params)
  n_time_periods = nrow(data_seq$explanatory_variables_transition)
    # p(data, latent state, time counter | parameters)
    alpha = lapply(
      preprocessed_tbf$semi_markov_depths_per_state,
      function(x){
        matrix(1/(x*length(preprocessed_tbf$semi_markov_depths_per_state)), 1, x)
      }
    )

    for(time_idx in seq(length(data_seq$emissions))){
      # transition
      if(time_idx>1){
        for(s in transition_model$dont_touch$latent_states_names){
          if(length(preprocessed_tbf$exit_states[[s]] > 0)){

          }
        }
      }
      # re-weighting by emission density
      for(var_idx in transition_model$dont_touch$latent_states_names){
        alpha[var_idx][] = alpha[var_idx][] +
          emission_model$dont_touch$log_likelihood(
            estimated_emission_param_vec = params$emission_params[,var_idx],
            fixed_emission_param_vec = emission_model$to_specify$fixed_emission_params[,var_idx],
            emission = data_seq$emissions[[time_idx]])
      }

    }
}
