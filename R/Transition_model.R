


EmptyList = function(latent_state_names){
  res = lapply(latent_state_names, function(x)NULL)
  names(res) = latent_state_names
  res}


MakeTBF = function(breakpoints = NULL){
  breakpoints = c(1, breakpoints)
  breakpoints = sort(unique(breakpoints))
  return(breakpoints)
}


PlotTBF = function(tbf, log.x = T){
  plot(1, 1, type = "n", ylim = c(0,1), xlim = c(1, max(max(tbf)*1.14, max(tbf) + 5)), log = c("", "x")[1+log.x],
       ylab = "temporal basis function value", xlab = "time counter")
  abline(h=1, col = "gray")
  abline(h=0, col = "gray")
  abline(v=0, col = "gray")
  abline(v=max(tbf), col = 1)
  abline(v=max(tbf), col = 1)
  text(x = max(max(tbf)*1.07, max(tbf) + 1), y = .5, labels = "constant after this time counter", srt = 90)
  if(length(tbf)==1) points(tbf, 1)
  if(length(tbf)>1){
    points(seq(tbf[1], tbf[2]), seq(1, 0, length.out = tbf[2]- tbf[1] +1), type = "b")
    points(seq(tbf[length(tbf)-1], tbf[length(tbf)]), seq(0, 1, length.out = tbf[length(tbf)]- tbf[length(tbf)-1] +1), type = "b")
  }
  if(length(tbf)>2){
    for(i in seq(2, length(tbf)-1)){
      points(seq(tbf[i-1], tbf[i]), seq(0, 1, length.out = tbf[i]- tbf[i-1] +1), type = "b")
      points(seq(tbf[i], tbf[i+1]), seq(1,0, length.out = tbf[i+1]- tbf[i] +1), type = "b")
    }
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

CreateTransitionParams = function(transition_model){
  transition_model_params = lapply(
    transition_model$latent_states_names, function(latent_state_name){
      lapply(transition_model$variable_basis_interactions[latent_state_name,],
             function(tbf_name){
               if(is.na(tbf_name))return(matrix(nrow = 0, ncol = 0))
               res = matrix(0,
                            nrow = length(transition_model$temporal_basis_function_list[[tbf_name]]+1),
                            ncol = sum(transition_model$possible_transitions[latent_state_name,])-1)
               colnames(res) = transition_model$latent_states_names[
                 setdiff(which(transition_model$possible_transitions[latent_state_name,]!=0), match(latent_state_name, transition_model$latent_states_names))
               ]
               return(res)
             })
    })
  names(transition_model_params) = transition_model$latent_states_names
  return(transition_model_params)
}



PlotTransitionGraph = function(transition_model){
  g = transition_model$possible_transitions
  diag(g) = 0
  g = igraph::graph_from_adjacency_matrix(g)
  plot(g)
}

AddEmissionModel = function(
    transition_model,
    fixed_emission_param_names = NULL,
    estimated_emission_param_names = NULL,
    explanatory_variable_names = NULL,
    observed_variables_names
    ){
  res = list()
  # necessary
  res$log_likelihood =      "function(obs,       estimated_emission_param, fixed_emission_param, emission_explanatory_variable)= (some log_likelihood for obs)"
  # can be obtained from log likelihood
  res$log_likelihood_grad = "function(obs,       estimated_emission_param, fixed_emission_param, emission_explanatory_variable) = (gradient of log_likelihood(obs) with respect to emission_param)"
  # must be coded by hand because mesures of random variables are not absolutely continuous
  res$observation_sampler = "function(n_samples, estimated_emission_param, fixed_emission_param, emission_explanatory_variable)"
  # necessary
  res$log_prior =           "function(estimated_emission_param_mat) = (some prior log-density)"
  # may be obtained using a MCMC
  res$log_prior_sampler =   "function(n_samples)"
  # creating matrix of fixed emission params at the right format. To be filled by the user
  if(!is.null(estimated_emission_param_names)){
    res$fixed_emission_params = matrix(NA, length(fixed_emission_param_names), length(transition_model$latent_states_names))
    row.names(res$fixed_emission_params) = fixed_emission_param_names
    colnames(res$fixed_emission_params) =  transition_model$latent_states_names
    message("A matrix of fixed emission parameters has been created. It must be filled by hand by the User.")
  }
  res$estimated_emission_param_names = estimated_emission_param_names
  res$explanatory_variable_names = explanatory_variable_names
  res$observerd_variables_names = observerd_variables_names
  return(list(transition = transition_model, emission = res))
}


data_list = 1
CheckDataList(data_list)
data_list = list(1, list())
CheckDataList(data_list)
data_list = list(list("Donald_Trump" = 1, "Vladimir_Putin" = 1, "Xi_Jing_Ping" = 1, "Emmanuel_Macron" = 1))
CheckDataList(data_list)
data_list = list(list("explanatory_variables_transition" = 1))
CheckDataList(data_list)
data_list = list(list("explanatory_variables_transition" = matrix(1)))
CheckDataList(data_list)
data_list = list(list("explanatory_variables_transition" = matrix("1")))
CheckDataList(data_list)
data_list = list(
  obs_1 = list("explanatory_variables_transition" = matrix(1)),
  obs_2 = list("explanatory_variables_transition" = matrix(1, 1, 2))
  )
CheckDataList(data_list)
data_list = list(
  obs_1 = list("explanatory_variables_transition" = matrix(nrow=10, ncol=0)),
  obs_2 = list("explanatory_variables_transition" = matrix(nrow=1, ncol=0))
  )
CheckDataList(data_list)
data_list = list(
  obs_1 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10)))),
  obs_2 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(11, 20))))
  )
CheckDataList(data_list)
data_list = list(
  obs_1 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10)))),
  obs_2 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))))
  )
CheckDataList(data_list)
data_list = list(
  obs_1 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = NULL),
  obs_2 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = NULL)
  )
CheckDataList(data_list)
data_list = list(
  obs_1 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = matrix(1, 1, 2)),
  obs_2 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = matrix(1))
  )
CheckDataList(data_list)

data_list = list(
  obs_1 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = matrix(1, 1, 2)),
  obs_2 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = matrix(1, 1, 2))
  )
CheckDataList(data_list)

CheckMatrixList = function(data_list, name){
  # checking that they are matrices
  if(any(sapply(data_list, function(x)!is.matrix(x[[name]])))){
    stop(paste("all", name, "must be matrices"))
  }
  # checking dimensions
  if(length(unique(sapply(data_list, function(x)ncol(x[[name]]))))>1){
    stop(paste("all", name, "must have the same number of columns"))
  }
  if(unique(sapply(data_list, function(x)ncol(x[[name]]))) == 0){
    stop(paste("all", name, "must have at least one column"))
  }
  # checking that they are numeric
  if(any(sapply(data_list, function(x)!is.numeric(x[[name]])))){
    stop(paste("all", name, "must be numeric, categorical data can be handeled using model.matrix()"))
  }
  # checking names
  if(length(unique(lapply(data_list, function(x)colnames(x[[name]]))))>1){
    stop(paste("all", name, "must have the same column names"))
  }
}

CheckDataList = function(data_list){
  # global format
  if(!is.list(data_list))stop("data_list must be a list (qui aurait pu prédire?)")
  if(any(!sapply(data_list, is.list)))stop("Every element of data_list must be a list")
  # checking names in every sublist
  if(any(sapply(data_list, function(x)!all(names(x)%in%c("explanatory_variables_emission", "explanatory_variables_transition", "emission")))))stop(
    "The possible names of an element from data_list are: explanatory_variables_emission, explanatory_variables_transition, emission"
  )

  # checking transition data
  CheckMatrixList(data_list, "explanatory_variables_transition")

  # checking emission data
  # checking that either all are NULL or all are non-NULL
  if(!(sum(sapply(data_list, function(x)is.null(x[["explanatory_variables_emission"]])))%in%c(0, length(data_list)))){
    stop("The explanatory_variables_emission must be either all NULL or all non-NULL")
  }
  if(!is.null(data_list[[1]][["explanatory_variables_emission"]])){
    CheckMatrixList(data_list, "explanatory_variables_emission")
  }

  # checking observations



  # check equality of sequences length

}

