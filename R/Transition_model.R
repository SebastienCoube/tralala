

#' data_list = 1
#' checkDataList(data_list)
#'
#' data_list = list(list(), list(), 1, list())
#' checkDataList(data_list)
#'
#' data_list = list(list("Donald_Trump" = 1, "Vladimir_Putin" = 1, "Xi_Jing_Ping" = 1, "Emmanuel_Macron" = 1), list("Donald_Trump" = 1, "Vladimir_Putin" = 1, "Xi_Jing_Ping" = 1, "Emmanuel_Macron" = 1))
#' checkDataList(data_list)
#'
#' data_list = list(list("explanatory_variables_transition" = 1))
#' checkDataList(data_list)
#'
#' data_list = list(list("explanatory_variables_transition" = matrix(1), "emissions" = list(1)))
#' checkDataList(data_list)
#'
#' data_list = list(
#'   obs_1 = list("explanatory_variables_transition" = matrix(1), "emissions" = list(1)),
#'   obs_2 = list("explanatory_variables_transition" = matrix(1, 1, 2), "emissions" = list(1))
#' )
#' checkDataList(data_list)
#'
#' data_list = list(
#'   obs_1 = list("explanatory_variables_transition" = matrix(nrow=10, ncol=0), "emissions" = list(1)),
#'   obs_2 = list("explanatory_variables_transition" = matrix(nrow=1, ncol=0), "emissions" = list(1))
#' )
#' checkDataList(data_list)
#'
#' data_list = list(
#'   obs_1 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))), "emissions" = list(1)),
#'   obs_2 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(11, 20))), "emissions" = list(1))
#' )
#' checkDataList(data_list)
#'
#' data_list = list(
#'   obs_1 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))), "emissions" = list(1)),
#'   obs_2 = list("explanatory_variables_transition" = matrix(seq(1000), ncol = 10, dimnames = list(NULL, seq(10))), "emissions" = list(1))
#' )
#' checkDataList(data_list)
#'
#'data_list = list(
#'   obs_1 = list("explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))), "emissions" = list(1)),
#'   obs_2 = list("explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))), "emissions" = list(1))
#' )
#' checkDataList(data_list)
#'
#' data_list = list(
#'   obs_1 = list("explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = NULL, emissions = lapply(seq(100), function(x)rnorm(1))),
#'   obs_2 = list("explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = NULL, emissions = lapply(seq(100), function(x)rnorm(1)))
#' )
#' checkDataList(data_list)
#'
#' data_list = list(
#'   obs_1 = list("explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = matrix(1, 1, 2), emissions = lapply(seq(100), function(x)rnorm(1))),
#'   obs_2 = list("explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = matrix(1), emissions = lapply(seq(100), function(x)rnorm(1)))
#' )
#' checkDataList(data_list)
#'
#' data_list = list(
#'   obs_1 = list("explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = matrix(1, 1, 2), emissions = lapply(seq(100), function(x)rnorm(1))),
#'   obs_2 = list("explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))), "explanatory_variables_emission" = matrix(1, 1, 2), emissions = lapply(seq(100), function(x)rnorm(1)))
#' )
#' checkDataList(data_list)
#'
#' data_list = list(
#'   obs_1 = list(
#'     "explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))),
#'     "explanatory_variables_emission" = matrix(rnorm(300), ncol = 3, dimnames = list(NULL, seq(3))),
#'     emissions = lapply(seq(100), function(x)rnorm(1))),
#'   obs_2 = list(
#'     "explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))),
#'     "explanatory_variables_emission" = matrix(rnorm(300), ncol = 3, dimnames = list(NULL, seq(3))),
#'     emissions = lapply(seq(100), function(x)rnorm(1)))
#' )
#' checkDataList(data_list)
#'
#' data_list = list(
#'   obs_1 = list(
#'     "explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))),
#'     emissions = lapply(seq(100), function(x)rnorm(1))),
#'   obs_2 = list(
#'     "explanatory_variables_transition" = matrix(rnorm(1000), ncol = 10, dimnames = list(NULL, seq(10))),
#'     emissions = lapply(seq(100), function(x)rnorm(1)))
#' )
#' checkDataList(data_list)

checkMatrixList = function(data_list, name){
  # checking that they are matrices
  issues =  sapply(data_list, function(x)!is.matrix(x[[name]]))
  if(any(issues)){
    stop(paste("all", name, "must be matrices, issues at", Reduce(function(x, y)paste(x, y, sep = ", "), which(issues))))
  }
  # checking dimensions
  if(length(unique(sapply(data_list, function(x)ncol(x[[name]]))))>1){
    stop(paste("all", name, "must have the same number of columns, but here we find", Reduce(function(x, y)paste(x, y, sep = ", "), unique(sapply(data_list, function(x)ncol(x[[name]]))))))
  }
  issues = sapply(data_list, function(x)ncol(x[[name]]) == 0)
  if(any(issues)){
    stop(paste("all", name, "must have at least one column, issues at", Reduce(function(x, y)paste(x, y, sep = ", "), which(issues))))
  }
  # checking that they are numeric
  issues = sapply(data_list, function(x)!is.numeric(x[[name]]))
  if(any(issues)){
    stop(paste("all", name, "must be numeric, categorical data can be handeled using model.matrix(), issues at", Reduce(function(x, y)paste(x, y, sep = ", "), which(issues))))
  }
  # checking names
  if(length(unique(lapply(data_list, function(x)colnames(x[[name]]))))>1){
    stop(paste("all", name, "must have the same column names"))
  }
  if(is.null(dimnames(data_list[[1]]$explanatory_variables_transition)[2])){
    stop(paste("all", name, "must have column names, corresponding to variable names"))
  }
  eigenvalues = eigen(crossprod(do.call(rbind, sapply(data_list, function(x)x[name]))))$values
  if(any(eigenvalues<0)) stop(paste(name, "appears to be degenerate, some variables are redundant"))
  if(max(eigenvalues)/min(eigenvalues) > 1e+16) stop(paste(name, "appears to be very poorly conditioned, some variables are redundant"))
}

checkDataList = function(data_list){
  # global format
  if(!is.list(data_list))stop("data_list must be a list (qui aurait pu prédire?)")
  issues = !sapply(data_list, is.list)
  if(any(issues))stop(paste("Every element of data_list must be a list, issues at", Reduce(function(x, y)paste(x, y, sep = ", "), which(issues))))
  # checking names in every sublist
  issues = sapply(data_list, function(x)!all(names(x)%in%c("explanatory_variables_emission", "explanatory_variables_transition", "emissions")))
  if(any(issues))stop(
    paste(
      "The possible names of an element from data_list are: explanatory_variables_emission, explanatory_variables_transition, emissions, issues at",
      Reduce(function(x, y)paste(x, y, sep = ", "), which(issues)))
  )

  # checking transition data
  checkMatrixList(data_list, "explanatory_variables_transition")

  # checking emission data
  # checking that either all are NULL or all are non-NULL
  if(!(sum(sapply(data_list, function(x)is.null(x[["explanatory_variables_emission"]])))%in%c(0, length(data_list)))){
    stop("The explanatory_variables_emission must be either all NULL or all non-NULL")
  }
  if(!is.null(data_list[[1]][["explanatory_variables_emission"]])){
    checkMatrixList(data_list, "explanatory_variables_emission")
  }

  # checking emissions
  if(any(!sapply(data_list, function(x)is.list(x$emissions)))){
    stop("emissions must be under the list format")
  }

  # check equality of sequences length
  # transition and emissions
  issues = sapply(data_list, function(x)length(x$emissions) != nrow(x$explanatory_variables_transition))
  if(any(issues)){
    stop(paste("the number of rows in explanatory_variables_transition and the length of emission must be equal, issues at", Reduce(function(x, y)paste(x, y, sep = ", "), which(issues))))
  }
  # transition and emissions explanatory variables
  if(!is.null(data_list[[1]][["explanatory_variables_emission"]])){
    issues = sapply(data_list, function(x)nrow(x$explanatory_variables_emission) != nrow(x$explanatory_variables_transition))
    if(any(issues)){
      stop(paste("the number of rows in explanatory_variables_transition and explanatory_variables_emission must be equal, issues at", Reduce(function(x, y)paste(x, y, sep = ", "), which(issues))))
    }
  }

  nvar_emission = ncol(data_list[[1]]$explanatory_variables_emission)
  if(is.null(data_list[[1]]$explanatory_variables_emission))nvar_emission = 0
  message(paste("data_list of", length(data_list), "sequences of observations with", ncol(data_list[[1]]$explanatory_variables_transition), "explanatory variables for the transition and", nvar_emission, "explanatory variables for the emission successfully checked"))
}




emptyList = function(latent_state_names){
  res = lapply(latent_state_names, function(x)NULL)
  names(res) = latent_state_names
  res}


#' makeTbf()
#' makeTbf(c(3, 10, 100))
#' makeTbf(c(-3, 10, 100))
#' makeTbf(c(3.5, 10, 100))

makeTbf = function(breakpoints = NULL){
  if(!is.null(breakpoints)){
    if(
      (!is.vector(breakpoints) | !all(breakpoints>0) | any(breakpoints - round(breakpoints) != 0))
    )stop("breakpoints must be a vector of positive integers or NULL")
  }
  breakpoints = c(1, breakpoints)
  breakpoints = sort(unique(breakpoints))
  return(breakpoints)
}


#' plotTbf(makeTbf(c(3, 10, 100, 1000)))
#' plotTbf(makeTbf())
plotTbf = function(tbf, log.x = T){
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


beginTransitionModel = function(
    data_list, temporal_basis_function_list, latent_states_names
){
  # getting the explanatory variable names from the data_list
  explanatory_variable_names = colnames(data_list[[1]]$explanatory_variables_transition)

  # graph of possible transitions between latent states
  possible_transitions = matrix(1, length(latent_states_names), length(latent_states_names))
  row.names(possible_transitions) = latent_states_names
  colnames(possible_transitions) = latent_states_names
  names(dimnames(possible_transitions)) = c("from", "to")
  message("By default, the matrix of authorized transitions between the latent states has been set to 1 everywhere. To forbid a transition, set the corresponding coefficient to 0")

  # table of interactions between explanatory variables and temporal basis functions
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
beginEmissionModel = function(
    latent_states_names,
    fixed_emission_param_names,
    estimated_emission_param_names
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
    res$fixed_emission_params = matrix(NA, length(fixed_emission_param_names), length(latent_states_names))
    row.names(res$fixed_emission_params) = fixed_emission_param_names
    colnames(res$fixed_emission_params) =  latent_states_names
    message("A matrix of fixed emission parameters has been created. It must be filled by hand by the User.")
  }
  res$estimated_emission_param_names = estimated_emission_param_names
  res$explanatory_variable_names = explanatory_variable_names
  res$observerd_variables_names = observerd_variables_names
  return(res)
}
beginModel = function(
    latent_states_names,
    data_list,
    temporal_basis_function_list,
    fixed_emission_param_names = NULL,
    estimated_emission_param_names = NULL
){
  checkDataList(data_list)
  res = list()
  res$transition_model = beginTransitionModel(
    data_list = data_list,
    temporal_basis_function_list = temporal_basis_function_list,
    latent_states_names = latent_states_names)
  res$emission_model = beginEmissionModel(
    data_list = data_list,
    fixed_emission_param_names = fixed_emission_param_names,
    estimated_emission_param_names = estimated_emission_param_names,
    latent_states_names = latent_states_names)
}


# Creates transition parameters with the right dimensions
CreateParams = function(model){
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



