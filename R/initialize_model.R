
# First step: Performing checks on the data and specifying latent sates names
initializeStep1 = function(
    latent_states,
    data_list
){
  res = list()
  # check data
  data_list = checkDataList(data_list)
  # initializing transition model
  res$transition_model = beginTransitionModel(
    explanatory_variable_names = colnames(data_list[[1]]$explanatory_variables),
    latent_states = latent_states)
  res$data_list = data_list
  message(
    "First step of model initialization done.\nGo to <your model>$transition_model$to_specify and modify what needs be.\nThen run initializeStep2."
  )
  res
}

# Second step: Checking the transition matrix between states, and the bases of temporal functions
initializeStep2 = function(model){
  # checking validity of transition matrix
  checkTransitionMat(model$transition_model)
  # checking bases of temporal functions
  checkBTF(model$transition_model)
  # design of potential interactions between BTFs and explanatory variables
  model$transition_model = createExplanatoryVariablesEffects(model$transition_model)
  message(
    "Second step of model initialization done.\nGo to <your model>$transition_model$to_specify and modify what needs be.\nThen run initializeStep3."
  )
  return(model)
}

# Third step: interactions between explanatory variables and latent state transitions
initializeStep3 = function(model){
  # processing some useful stuff
  model$transition_model = c(model$transition_model$dont_touch, model$transition_model$to_specify)
  model$transition_model$misc$outstates =
    apply(model$transition_model$possible_transitions, 1, function(x)model$transition_model$latent_states[which(x)])
  model$transition_model$misc$outstates = mapply(
    setdiff, model$transition_model$misc$outstates, model$transition_model$latent_states
  )
  model$transition_model$misc$number_outstates = sapply(model$transition_model$misc$outstates, length)
  model$transition_model$misc$is_state_absorbing = sapply(model$transition_model$misc$outstates, function(x)length(x)==0)
  model$transition_model$misc$non_absorbing_states = model$transition_model$latent_states[!model$transition_model$misc$is_state_absorbing]
  model$transition_model$misc$active_explanatory_variables =
    lapply(
      model$transition_model$explanatory_variables_effects,
      function(x){
        if(is.null(x))return(NULL)
        names(x)[which(x)]
        })
  model$transition_model$BTF_per_state <-
    lapply(model$transition_model$BTF_per_state, function(x) c(1,x))
  model$transition_model$misc$number_active_explanatory_variables <-
    sapply(model$transition_model$misc$active_explanatory_variables, length)
  model$transition_model$misc$depth <- sapply(model$transition_model$BTF_per_state, max)
  model$transition_model$misc$BTFdim <- sapply(model$transition_model$BTF_per_state, length)
  model$transition_model$misc$BTFstepsize <- lapply(model$transition_model$BTF_per_state, diff)
  emission_explanatory_variables = sapply(model$transition_model$explanatory_variable_names, function(X)FALSE)
  emission_explanatory_variables[1] = TRUE
  model$emission_model = list(
    "to_specify" = list(
      emission_explanatory_variables = emission_explanatory_variables,
      emission_parameters_names = c()
      ))
  message(
    "Third step of model initialization done, the transition model is done.\nNow, the *emission model* is starting.\nGo to <your model>$emission_model$to_specify and modify what needs be.\nThen run initializeStep4."
  )
  return(model)
}

# Fourth step: checking emission parameters names and explanatory variables
initializeStep4 = function(model){
  # Checking emission parameters names
  if(!all(is.character(model$emission_model$to_specify$emission_parameters_names))){
    stop("model$emission_model$to_specify$emission_parameters_names must be a vector of character")
  }
  model$emission_model$dont_touch <- list(
    emission_explanatory_variables = names(model$emission_model$to_specify$emission_explanatory_variables)[model$emission_model$to_specify$emission_explanatory_variables],
    emission_parameters_names = model$emission_model$to_specify$emission_parameters_names,
    number_emission_parameters = length(model$emission_model$to_specify$emission_parameters_names)
  )
  model$emission_model$to_specify <- list(
    emission_log_likelihood =                    "function(emission, emission_parameters_vector)",
    emission_log_likelihood_grad_NOT_MANDATORY = "function(emission, emission_parameters_vector)",
    emission_regression_coefficients_log_prior = "function(emission_regression_coefficients)",
    regression_coefficients_array_NOFILL = createRandomEmissionCoeffs(
      emission_parameters = model$emission_model$dont_touch$emission_parameters_names,
      explanatory_variables = model$emission_model$dont_touch$emission_explanatory_variables,
      latent_states = model$transition_model$latent_states)
  )
  message(
    "Fourth step of model initialization done.\nGo to <your model>$emission_model$to_specify and modify what needs be.\nThen run initializeStep5."
  )
  return(model)
}


# Fifth step: checking expected return of the emission likelihood and prior
initializeStep5 = function(model, n_tests = 5, seed = 1){
  # checking arguments of emission prior and ll
  if(formalArgs(model$emission_model$to_specify$emission_regression_coefficients_log_prior)!="emission_regression_coefficients")stop("Emission log-prior must have `emission_regression_coefficients` as argument")
  if(!identical(
    sort(match(formalArgs(model$emission_model$to_specify$emission_log_likelihood),
               c("emission", "emission_parameters_vector"))),
    seq(2))){
    stop("Emission log-likelihood must have `emission` and `emission_parameters_vector` as arguments")
  }

  # checking arguments of emission ll gradient if it is specified
  if(is.function(model$emission_model$to_specify$emission_log_likelihood_grad_NOT_MANDATORY)){
    if(!identical(
      sort(match(formalArgs(model$emission_model$to_specify$emission_log_likelihood_grad_NOT_MANDATORY),
                 c("emission", "emission_parameters_vector"))),
      seq(2))){
      stop("Gradient of emission log-likelihood must have `emission` and `emission_parameters_vector` as arguments")
    }
  }

  # testing that the emission function is right
  message("Testing emission parameters prior, emission log likelihood, and its gradient if provided")
  set.seed(1)
  # creating random valued regression coefficients for the emission parameters
  # and testing the prior
  for(test_idx in seq(n_tests)){
    emission_regression_coefficients <- createRandomEmissionCoeffs(
      emission_parameters = model$emission_model$dont_touch$emission_parameters_names,
      explanatory_variables = model$emission_model$dont_touch$emission_explanatory_variables,
      latent_states = model$transition_model$latent_states)
    prior_eval <- model$emission_model$to_specify$emission_regression_coefficients_log_prior(emission_regression_coefficients)
    if(!is.numeric(prior_eval)|length(prior_eval)>1)stop("The emission log prior should return a numeric of length 1")
  }
  # creating random valued emission parameters,
  # and testing the log likelihood and its gradient
  # creating emission gradient
  emissionLogLikFiniteDiff <- getFiniteDiffGradEmissionLL(model$emission_model$to_specify$emission_log_likelihood)
  N <- sum(sapply(model$data_list, function(x)length(x$emissions)))
  gradient_check <- matrix(0, n_tests * N, model$emission_model$dont_touch$number_emission_parameters)
  i = 0
  colnames(gradient_check) <- model$emission_model$dont_touch$emission_parameters_names
  for(test_idx in seq(n_tests)){
    test_emission_parameters <- rnorm(
      model$emission_model$dont_touch$number_emission_parameters, 0, 10)
    names(test_emission_parameters) <- model$emission_model$dont_touch$emission_parameters_names
    # looping over individuals and time periods
    for(ind_idx in seq_len(length(model$data_list))){
      for(time_idx in seq(length(model$data_list[[ind_idx]]$emissions))){
        i = i+1
        # evaluating likelihood
        likelihood_eval <-
          model$emission_model$to_specify$emission_log_likelihood(
            emission = model$data_list[[ind_idx]]$emissions[[time_idx]],
            emission_parameters_vector = test_emission_parameters
          )
        if(!is.numeric(likelihood_eval)|length(likelihood_eval)>1)stop("The emission log likelihood should return a numeric of length 1")
        # evaluating gradient
        if(is.function(model$emission_model$to_specify$emission_log_likelihood_grad_NOT_MANDATORY)){
          analytic_grad <- model$emission_model$to_specify$emission_log_likelihood_grad_NOT_MANDATORY(
            emission = model$data_list[[ind_idx]]$emissions[[time_idx]],
            test_emission_parameters)
          finite_diff_grad <- emissionLogLikFiniteDiff(
            emission = model$data_list[[ind_idx]]$emissions[[time_idx]],
            test_emission_parameters)
          if(!is.numeric(analytic_grad))stop("The analytic gradient must return a numeric vector")
          if(!identical(names(analytic_grad), model$emission_model$dont_touch$emission_parameters_names)){
            stop(paste("The analytic gradient must return a named vector, whose names are the emission parameters:",
                       Reduce(paste, model$emission_model$dont_touch$emission_parameters_names)))
          }
          gradient_check[i, ] <- (analytic_grad - finite_diff_grad)/(finite_diff_grad+1e-6)
        }
      }
    }
  }
  # printing results of gradient check vs finite diff
  if(is.function(model$emission_model$to_specify$emission_log_likelihood_grad_NOT_MANDATORY)){
    message("Distributions of relative difference between finite difference and analytic gradients")
    gradient_check <- apply(gradient_check, 2, function(x)quantile(x, c(0, .025, .1, .5, .9, .975, 1)))
    gradient_check <- round(gradient_check, 3)
    print(gradient_check)
  }
  if(!is.function(model$emission_model$to_specify$emission_log_likelihood_grad_NOT_MANDATORY)){
    model$emission_model$to_specify$emission_log_likelihood_grad_NOT_MANDATORY <-
      emissionLogLikFiniteDiff
  }
  model$emission_model = c(
    emission_regression_coefficients_log_prior =
      model$emission_model$to_specify$emission_regression_coefficients_log_prior,
    emission_log_likelihood =
      model$emission_model$to_specify$emission_log_likelihood,
    emission_log_likelihood_grad =
      model$emission_model$to_specify$emission_log_likelihood_grad_NOT_MANDATORY,
    model$emission_model$dont_touch)
  message("Last step of model initialization done, ready for fit.")
  return(model)
}







