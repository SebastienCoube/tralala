

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
    emission_log_likelihood,
    emission_log_likelihood_grad = NULL,
    emission_log_prior,
    verbose = T){
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
  if(verbose){
    message(paste("The function emission_log_likelihood has been checked succesfully, but you have to make sure that:
  - It is a log-density for the emissions (log-likelihood) for any set of", nestimated,"real-valued estimated parameters", nfixed ,"real-valued fixed parameters, and", nvar, "explanatory variables
  - It is defined and twice-differentiable for any real-valued vector of estimated_emission_param_vec."))
  }
  # gradient
  if(is.null(emission_log_likelihood_grad)){
    if(verbose){message("The argument emission_log_likelihood_grad was not provided and is obtained by applying finite differences on emission_log_likelihood")}
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
  if(verbose){
    message("emission_log_likelihood_grad checked succesfully")
    message("----------")
  }

  # Checking the function provided for the emission prior
  checkEmissionLogPrior(emission_log_prior, "emission_log_prior")
  if(verbose){
    message(paste("The function emission_log_prior has been checked succesfully, but you have to make sure that it is an actual log-density over the", length(latent_states_names), "x", length(estimated_emission_param_names), "estimated emission parameters" ))
  }
  emission_log_prior_grad = function(estimated_emission_param_mat){
    res = 0*estimated_emission_param_mat
    fx = emission_log_prior(estimated_emission_param_mat)
    params_ = estimated_emission_param_mat
    for(i in seq(length(res))){
      params_[i] = params_[i] + 1e-6
      res[i] =  1e6*(emission_log_prior(params_)-fx)
      params_[i] = estimated_emission_param_mat[i]
    }
  }
  checkEmissionLogPrior(emission_log_prior_grad, "emission_log_prior_grad")
  if(verbose){
    message("emission_log_prior_grad checked succesfully")
    message("----------")
  }


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

#groups = list(c("S", "R"), "D", "I")
#x = rnorm(4)
#names(x) = c("S", "I", "R", "D")
#mar_var = c(1,1,1)
#mean = rep(0, 3)
#DMVNGroup(x, mean, mar_var, groups)

DMVNGroup = function(
    x,
    mean,
    mar_var,
    groups
){
  if(length(mean)!=length(groups))stop("groups and mean must have the same length")
  if(length(mar_var)!=length(groups))stop("groups and mar_var must have the same length")
  if(!is.list(groups))stop("groups must be a list")
  if(any(is.na(match(do.call(c, groups), names(x)))))stop("The groups and the names in x do not match")
  if(any(is.na(match(names(x), do.call(c, groups)))))stop("The groups and the names in x do not match")
  if(any(mar_var<0))stop("mar_var must be positive")
  meanvec = x
  Sig = matrix(0, length(x), length(x)); row.names(Sig) = names(x); colnames(Sig) = names(x)
  for(i in seq(length(groups))){
    meanvec[groups[[i]]] = mean[i]
    Sig[groups[[i]], groups[[i]]] = mar_var[i]*.99
    Sig[cbind(groups[[i]], groups[[i]])] = mar_var[i]
  }
  return(-.5 * sum(((x - meanvec) %*% solve(Sig)) * (x - meanvec)))
}

#'plot(seq(-20, 20), softPlus(seq(-20, 20)))
#'abline(a=0, b=1)
#'abline(h=0)
softPlus = function(x) log(1 + exp(x))

#'plot(seq(-20, 20), softPlusSquare(seq(-20, 20)))
#'abline(a=0, b=1)
#'abline(h=0)
softPlusSquare = function(x) log(1 + exp(x))^2


