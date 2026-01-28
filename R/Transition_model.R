

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
  message(paste("data_list successfully checked, there are", length(data_list), "sequences of observations, ", ncol(data_list[[1]]$explanatory_variables_transition), "explanatory variables for the transition, ", nvar_emission, "explanatory variables for the emission."))
  message("----------")
}


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
  # sanity check and reformatting of tbf list
  temporal_basis_function_list = lapply(temporal_basis_function_list, makeTbf)
  if(is.null(names(temporal_basis_function_list)))stop("temporal_basis_function_list must be a named list.")
  message(paste("temporal_basis_function_list successfully checked, there are", length(temporal_basis_function_list), "temporal basis functions"))
  message("----------")

  # getting the explanatory variable names from the data_list
  explanatory_variable_names = colnames(data_list[[1]]$explanatory_variables_transition)

  # graph of possible transitions between latent states
  possible_transitions = matrix(1, length(latent_states_names), length(latent_states_names))
  row.names(possible_transitions) = latent_states_names
  colnames(possible_transitions) = latent_states_names
  names(dimnames(possible_transitions)) = c("from", "to")
  message(paste("Authorized transition matrix between the ", length(latent_states_names), " latent states created.
  - Filled with 1's everywhere, all transitions are possible
  - To forbid a transition between two latent states, set the corresponding coefficient to 0"
  ))
  message("----------")
  # table of interactions between explanatory variables and temporal basis functions
  variable_basis_interactions = matrix(1, length(latent_states_names), length(explanatory_variable_names))
  row.names(variable_basis_interactions) = latent_states_names
  colnames(variable_basis_interactions) = explanatory_variable_names
  variable_basis_interactions[] = NA
  names(dimnames(variable_basis_interactions)) = c("origin_latent_state", "explanatory_variable")
  message("Table of interactions between ", length(latent_states_names), " latent states and " , length(temporal_basis_function_list),  " temporal basis functions created.
  -Filled with NA's everywhere.
  -To make an explanatory variable interact with a basis, put the name of the chosen basis to the corresponding slot.
  -To `kill' the variable, leave the corresponding slot to NA
  -Each row of the table must have at least one non-NA slot"
  )
  message("----------")

  return(list(
    to_specify =
      list(
        "possible_transitions" = possible_transitions,
        "variable_basis_interactions" = variable_basis_interactions
      ),
    dont_touch =
      list(
        "latent_states_names" = latent_states_names,
        "temporal_basis_function_list" = temporal_basis_function_list,
        "explanatory_variable_names" = explanatory_variable_names)
  ))
}


#' emission_log_likelihood is a function that takes as arguments:
#' - estimated_emission_param_vec, a real-valued vector of emission parameters who are estimated by the model
#' - fixed_emission_param_vec,     a real-valued vector of emission parameters who are fixed by the user
#' - emission, an observation from the emissions slot of an element from data_list
#' - explanatory_variable_vec, a real-valued vector taken as a row of explanatory_variables_emission from an element from data_list
#' Important note: the function should be evaluable and twice differentiable for any value of estimated_emission_param_vec.
#' Be careful with parameters such as a standard deviation, a scale, and whatnot, who are positive. Those must be passed to the log !
#' estimated_emission_param_vec and fixed_emission_param_vec are the parameters associated with one label of the latent state

#' emission_log_prior is a function that takes as arguments:
#' - estimated_emission_param_vec, a real-valued vector of emission parameters who are estimated by the model
#' Important note: the function should be evaluable and twice differentiable for any value of estimated_emission_param_vec
#' Be careful with parameters such as a standard deviation, a scale, and whatnot, who are positive. Those must be passed to the log !
#' Also be careful with hard constraints such as "this parameter must be greater than that parameter".
#' This breaks the differentiability requirement, and can also lead to monstruous posteriors (see Robert, Marin, Mergensen)
#' Last important note : to ensure the posterior being valid, the prior must be a valid distribution as well.


#'emission_log_likelihood =      function(estimated_emission_param_vec, fixed_emission_param_vec, emission, explanatory_variable_vec){
#'}
#'emission_log_likelihood_grad = function(estimated_emission_param_vec, fixed_emission_param_vec, emission, explanatory_variable_vec){
#'}
#'emission_log_prior = function(estimated_emission_param_mat){
#'}

checkEmissionLogLikelihood = function(f, name){
  if(!is.function(f))stop(paste(name, " must be a function"))
  if(any(is.na(match(formalArgs(f), c("estimated_emission_param_vec", "fixed_emission_param_vec", "emission", "explanatory_variable_vec"))))){
    stop(paste("The formal arguments of the function", name, "must be: estimated_emission_param_vec, fixed_emission_param_vec, emission, and optionally explanatory_variable_vec"))
  }
  if(any(is.na(match(c("estimated_emission_param_vec", "fixed_emission_param_vec", "emission"), formalArgs(f))))){
    stop(paste("The formal arguments of the function", name, "must be: estimated_emission_param_vec, fixed_emission_param_vec, emission, and optionally explanatory_variable_vec"))
  }
}
checkEmissionLogPrior = function(f, name){
  if(!is.function(f))stop(paste(name, " must be a function"))
  if(formalArgs(f) != "estimated_emission_param_mat"){
    stop(paste("The formal arguments of the function", name, "must be: estimated_emission_param_mat"))
  }
}

beginEmissionModel = function(
    data_list,
    latent_states_names,
    fixed_emission_param_names,
    estimated_emission_param_names,
    emission_log_likelihood,  emission_log_likelihood_grad = NULL,
    emission_log_prior, emission_log_prior_grad = NULL
){

  # Log Likelihood of the emissions

  # checking parameters
  if(is.null(estimated_emission_param_names) & is.null(fixed_emission_param_names))stop("estimated_emission_param_names and fixed_emission_param_names cannot both be NULL")
  # Checking the function provided for the emission log-likelihood
  checkEmissionLogLikelihood(emission_log_likelihood, "emission_log_likelihood")
  # messaging
  nvar = ncol(data_list[[1]]$explanatory_variables_emission)
  if(is.null(nvar))nvar = 0
  nfixed = length(fixed_emission_param_names)
  if(is.null(nfixed))nfixed = 0
  nestimated = length(estimated_emission_param_names)
  if(is.null(nestimated))nestimated = 0
  message(paste("The function emission_log_likelihood has been checked succesfully, but you have to make sure that:
  - It is a log-density for the emissions (log-likelihood) for any set of", nestimated,"real-valued estimated parameters", nfixed ,"real-valued fixed parameters, and", nvar, "explanatory variables
  - It is defined and twice-differentiable for any real-valued vector of estimated_emission_param_vec"))
  # gradient
  if(is.null(emission_log_likelihood_grad)){
    message("emission_log_likelihood_grad was not provided and is obtained by applying finite differences on emission_log_likelihood")
    emission_log_likelihood_grad = function(estimated_emission_param_vec, fixed_emission_param_vec, emission, explanatory_variable_vec){
      res = rep(0, length(estimated_emission_param_vec))
      fx = emission_log_likelihood(estimated_emission_param_vec, fixed_emission_param_vec, emission, explanatory_variable_vec)
      params_ = estimated_emission_param_vec
      for(i in seq(length(res))){
        params_[i] = params_[i] + 1e-6
        res[i] =  1e6*(emission_log_likelihood(params_, fixed_emission_param_vec, emission, explanatory_variable_vec)-fx)
        params_[i] = estimated_emission_param_vec[i]
      }
      return(res)
    }
  }
  checkEmissionLogLikelihood(emission_log_likelihood_grad, "emission_log_likelihood_grad")
  message("emission_log_likelihood_grad checked succesfully")
  message("----------")

  # Checking the function provided for the emission prior
  checkEmissionLogPrior(emission_log_prior, "emission_log_prior")
  message(paste("The function emission_log_prior has been checked succesfully, but you have to make sure that it is an actual log-density over the", length(latent_states_names), "x", length(estimated_emission_param_names), "estimated emission parameters" ))
  if(is.null(emission_log_prior_grad)){
    message("emission_log_prior_grad obtained by applying finite differences on emission_log_prior")
    emission_log_prior_grad = function(estimated_emission_param_mat){
      res = 0*estimated_emission_param_mat
      fx = emission_log_prior(estimated_emission_param_mat)
      params_ = estimated_emission_param_mat
      for(i in seq(length(res))){
        params_[i] = params_[i] + 1e-6
        res[i] =  1e6*(emission_log_prior(params_)-fx)
        params_[i] = estimated_emission_param_mat[i]
      }
      return(res)
    }
  }
  checkEmissionLogPrior(emission_log_prior_grad, "emission_log_prior_grad")
  message("emission_log_prior_grad checked succesfully")
  message("----------")


  res = list()
  res$dont_touch = list()
  res$dont_touch$log_likelihood = emission_log_likelihood
  res$dont_touch$log_likelihood_grad = emission_log_likelihood_grad
  res$dont_touch$log_prior = emission_log_prior
  res$dont_touch$log_prior_grad = emission_log_prior_grad
  res$dont_touch$estimated_emission_param_names = estimated_emission_param_names
  # creating matrix of fixed emission params at the right format. To be filled by the user
  if(!is.null(estimated_emission_param_names)){
    res$to_specify = list()
    fixed_emission_params = matrix(NA, length(fixed_emission_param_names), length(latent_states_names))
    row.names(fixed_emission_params) = fixed_emission_param_names
    colnames(fixed_emission_params) =  latent_states_names
    message(paste("Matrix of", length(fixed_emission_param_names), "x", length(latent_states_names),  "fixed emission parameters created.
  - Filled with NAs
  - You must fill it by hand with the desired parameters"))
    res$to_specify$fixed_emission_params = fixed_emission_params
  }
  return(res)
}

beginModel = function(
    latent_states_names,
    data_list,
    temporal_basis_function_list,
    fixed_emission_param_names,
    estimated_emission_param_names,
    emission_log_likelihood,  emission_log_likelihood_grad = NULL,
    emission_log_prior, emission_log_prior_grad = NULL

){
  res = list()
  # check data
  checkDataList(data_list)
  # transition model
  res$transition_model = beginTransitionModel(
    data_list = data_list,
    temporal_basis_function_list = temporal_basis_function_list,
    latent_states_names = latent_states_names)
  # emission model
  res$emission_model = beginEmissionModel(
    data_list,
    latent_states_names = latent_states_names,
    fixed_emission_param_names = fixed_emission_param_names,
    estimated_emission_param_names = estimated_emission_param_names,
    emission_log_likelihood = emission_log_likelihood,  emission_log_likelihood_grad = emission_log_likelihood_grad,
    emission_log_prior = emission_log_prior, emission_log_prior_grad = emission_log_prior_grad
  )
  res
}


# Creates transition parameters with the right dimensions
createParams = function(model){
  transition_model_params = lapply(
    model$transition_model$dont_touch$latent_states_names, function(latent_state_name){
      lapply(model$transition_model$to_specify$variable_basis_interactions[latent_state_name,],
             function(tbf_name){
               if(is.na(tbf_name))return(NULL)
               res = matrix(0,
                            nrow = length(model$transition_model$dont_touch$temporal_basis_function_list[[tbf_name]]+1),
                            ncol = sum(model$transition_model$to_specify$possible_transitions[latent_state_name,])-1)
               colnames(res) = model$transition_model$dont_touch$latent_states_names[
                 setdiff(which(model$transition_model$to_specify$possible_transitions[latent_state_name,]!=0), match(latent_state_name, model$transition_model$dont_touch$latent_states_names))
               ]
               return(res)
             })
    })
  names(transition_model_params) = model$transition_model$dont_touch$latent_states_names
  emission_params = matrix(0, length(model$emission_model$dont_touch$estimated_emission_param_names), length(model$transition_model$dont_touch$latent_states_names))
  colnames(emission_params) = model$transition_model$dont_touch$latent_states_names
  row.names(model$emission_model$dont_touch$estimated_emission_param_names) = model$emission_model$dont_touch$estimated_emission_param_names
  return(list("transition_params" = transition_model_params, "emission_params" = emission_params))
}

plotTransitionGraph = function(model){
  g = model$transition_model$to_specify$possible_transitions
  diag(g) = 0
  g = igraph::graph_from_adjacency_matrix(g)
  plot(g)
}



