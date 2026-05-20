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

# checks that the data is under the right format
checkDataList = function(data_list){
  # global format
  if(!is.list(data_list))stop("data_list must be a list (qui aurait pu prédire?)")
  issues = !sapply(data_list, is.list)
  if(any(issues))stop(paste("Every element of data_list must be a list, issues at", Reduce(function(x, y)paste(x, y, sep = ", "), which(issues))))
  # checking names in every sublist
  issues = sapply(data_list, function(x)!all(names(x)%in%c("explanatory_variables_emission", "explanatory_variables_transition", "emissions", "latent_states")))
  if(any(issues))stop(
    paste(
      "The possible names of an element from data_list are: explanatory_variables_emission, explanatory_variables_transition, emissions, latent_states (for simulated data), issues at",
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
  issues = sapply(data_list, function(x)length(x$emissions) != (nrow(x$explanatory_variables_transition)+1))
  if(any(issues)){
    stop(paste("the number of rows in explanatory_variables_transition minus one and the length of emission must be equal, issues at", Reduce(function(x, y)paste(x, y, sep = ", "), which(issues))))
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
