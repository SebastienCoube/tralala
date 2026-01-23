


EmptyList = function(latent_state_names){
  res = lapply(latent_state_names, function(x)NULL)
  names(res) = latent_state_names
  res}


ProcessBreakpoints = function(breakpoints = NULL){
  if(any(breakpoints == 1))stop("1 cannot be a breakpoint")
  breakpoints = sort(unique(breakpoints))
  if(is.null(breakpoints))breakpoints=1
  return(breakpoints)
}



PlotTentTbf = function(breakpoints){
  if(is.null(breakpoints)){
    plot(
      seq(12),
      rep(1, 12),
      xlab = "time counter variable",
      ylab = "temporal basis function",
      ylim = c(0,1.2),
      xlim = c(1,10),
      type = "l")
    points(c(1), c(1))
    abline(h=0)
  }
  if(!is.null(breakpoints)){
  plot(
    c(1, breakpoints, max(breakpoints)+.1*max(breakpoints)),
    rep(1, length(breakpoints)+2),
    xlab = "time counter variable",
    ylab = "temporal basis function",
    ylim = c(0,1),
    type = "n")
    lines(c(1, 2*max(breakpoints) + 10), c(1,1))
    for(i in seq(length(breakpoints))){
      tilt = .000
      lines(c(
        c(1, breakpoints[-length(breakpoints)])[i],
        breakpoints[i]),
        c(1, 0) + tilt, col= i+1)
    }
    if(length(breakpoints>1)){
      for(i in seq(length(breakpoints)-1)){
        tilt = .000
        lines(c(
          c(breakpoints[-length(breakpoints)])[i],
          breakpoints[i]),
          c(1, 0) + tilt, col= i+2)
      }
    }
    abline(h=0, col = "grey")
    abline(v=0, col = "grey")

  }


}

BeginTransitionModel = function(
    explanatory_variable_names, temporal_basis_function_list, latent_states_names
){
  possible_transitions = matrix(1, length(latent_states_names), length(latent_states_names))
  row.names(possible_transitions) = latent_states_names
  colnames(possible_transitions) = latent_states_names
  names(dimnames(possible_transitions)) = c("from", "to")
  message("By default, the matrix of authorized transitions between the latent states has been set to 1 everywhere. To forbid a transition, set the corresponding coefficient to 0")
  variable_basis_interactions = matrix(1, length(latent_states_names), length(explanatory_variable_names))
  row.names(variable_basis_interactions) = latent_states_names
  colnames(variable_basis_interactions) = explanatory_variable_names
  variable_basis_interactions[] = names(temporal_basis_function_list)[1]
  names(dimnames(variable_basis_interactions)) = c("origin_latent_state", "explanatory_variable")
  message("By default, the matrix of variable and temporal basis interaction has been filled with the name of the first temporal basis.
        -To make the explanatory variable interact with another basis, change the name in the corresponding slot.
        -To `kill' the variable, set the corresponding slot to NA")
  return(list("possible_transitions" = possible_transitions,
              "variable_basis_interactions" = variable_basis_interactions,
              "latent_states_names" = latent_states_names,
              "temporal_basis_function_list" = temporal_basis_function_list,
              "explanatory_variable_names" = explanatory_variable_names))
}

AddTransitionParams = function(begun_transition_model){
  begun_transition_model$transition_model_params = lapply(
    begun_transition_model$latent_states_names, function(latent_state_name){
      lapply(begun_transition_model$variable_basis_interactions[latent_state_name,],
             function(tbf_name){
               if(is.na(tbf_name))return(matrix(nrow = 0, ncol = 0))
               res = matrix(0,
                            nrow = length(begun_transition_model$temporal_basis_function_list[[tbf_name]]+1),
                            ncol = sum(begun_transition_model$possible_transitions[latent_state_name,])-1)
               colnames(res) = begun_transition_model$latent_states_names[
                 setdiff(which(begun_transition_model$possible_transitions[latent_state_name,]!=0), match(latent_state_name, begun_transition_model$latent_states_names))
                 ]
               return(res)
             })
    })
  names(begun_transition_model$transition_model_params) = begun_transition_model$latent_states_names
  return(begun_transition_model)
}



Softmax = function(multiplied_tbfs)

sample_latent_state = function(transition_model, explanatory_variables, starting_states, seed = 1){
  set.seed(seed)
  latent_state = matrix(0, nrow(explanatory_variables), length(starting_states))
  counter_var =  matrix(0, nrow(explanatory_variables), length(starting_states))
  counter_var[1,]=1
  t=2
  # multiplicating each tbf by the corresponding parameter to save time
  tbf_times_params = list()
  for(origin_latent_state in row.names(transition_model$variable_basis_interactions)){
    tbf_times_params[[origin_latent_state]] = list()
    for(expanatory_variable in colnames(transition_model$variable_basis_interactions)){
      tbf_times_params[[origin_latent_state]][[expanatory_variable]] = NA
      chosen_basis= transition_model$variable_basis_interactions[origin_latent_state,expanatory_variable]
      if(!is.na(chosen_basis)){
        tbf_times_params[[origin_latent_state]][[expanatory_variable]] =
          transition_model$temporal_basis_function_list[[chosen_basis]] %*%
          transition_model$transition_model_params[[origin_latent_state]][[expanatory_variable]]
      }
    }
  }

  for(t in seq(2, nrow(explanatory_variables))){

    latent_state[t,] =
mapply(
  function(current_latent_state, current_counter_var){
    # latent states that can be reached from this one
    possible_new_latent_states = transition_model$latent_states_names[transition_model$possible_transitions[current_latent_state,]==1]
    # names of temporal basis functions that interact with the covariates
    implied_tbfs = as.vector(na.omit(transition_model$variable_basis_interactions[current_latent_state,]))
    unique_implied_tbfs = unique(implied_tbfs)
    # extracting the right rows from the tbfs using the coutner variable
    unique_implied_tbfs_rows = lapply(unique_implied_tbfs, function(tbf_name)transition_model$temporal_basis_function_list[[tbf_name]][min(current_counter_var, nrow(transition_model$temporal_basis_function_list[[tbf_name]])),])
    names(unique_implied_tbfs_rows) = unique_implied_tbfs
    # getting the
    implied_covariates = transition_model$variable_basis_interactions[current_latent_state,]
  },
  current_latent_state = latent_state[t-1,],
  current_counter_var = counter_var[t-1,]
)
  }
}


PlotTransitionGraph = function(transition_model){
  g = transition_model$possible_transitions
  diag(g) = 0
  g = igraph::graph_from_adjacency_matrix(g)
  plot(g)
}

InitializeEmissionModel = function(transition_model){
  res = list()
  res$likelihood = NA
  res$generator = NA
  res$parameters = EmptyList(transition_model$latent_states_names)
  res$prior = EmptyList(transition_model$latent_states_names)
  return(res)
}


MultiplyTentFunction = function(transition_model){
  variable_basis_interactions = transition_model$variable_basis_interactions
  transition_model_params = transition_model$transition_model_params
  temporal_basis_function_list = transition_model$temporal_basis_function_list
}





MultiplyTentFunction = function(transition_model_params, temporal_basis_function_list, variable_basis_interactions){
  res = list()
  for(i in seq(nrow(variable_basis_interactions))){
    res[[row.names(variable_basis_interactions)[i]]] = list()
    for(j in seq(ncol(variable_basis_interactions))){
      res[[row.names(variable_basis_interactions)[i]]][[colnames(variable_basis_interactions)[j]]] = NA
      if(!is.na(variable_basis_interactions[i,j])){
      param = transition_model_params[[row.names(variable_basis_interactions)[i]]][[colnames(variable_basis_interactions)[j]]]


      tbf = temporal_basis_function_list[[variable_basis_interactions[i,j]]]
      res[[row.names(variable_basis_interactions)[i]]][[colnames(variable_basis_interactions)[j]]] =
      mapply(
        beginsegment = c(1, tbf[-length(tbf)]),
        endsegment = tbf,
        param = param
      )

      }
    }
  }
}
