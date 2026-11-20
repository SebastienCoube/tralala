valueAtBtfKnots = function(explanatory_variable, transition_param){
  t(apply(
    apply(transition_param, c(1,3), function(x)sum(x*explanatory_variable[dimnames(transition_param)[[2]]])),
    1, softmax))
}

#' @export
softmax = function(x){
  res = c(1, exp(x))
  res = res/sum(res)
  res
}

normalize = function(x){x / sum(x)}


# addProbs(log(.5), log(.5)) - log(1)
# addProbs(log(.25), log(.25)) - log(.5)
# addProbs(-Inf, log(.5)) - log(.5)
addProbs <- function(log_prob_1, log_prob_2){
  maxlogprob <- pmax(log_prob_1, log_prob_2)
  return(
    log(
        exp(log_prob_1 - maxlogprob) +
        exp(log_prob_2 - maxlogprob)
    ) + maxlogprob
  )
}

# collapse log-probs into ll
collapseLogProbs <-  function(log_probs){
  if(is.list(log_probs[[1]]))return(lapply(log_probs, collapseLogProbs))
  if(is.null(dim(log_probs[[1]])))return(sumLogProbs(abind::abind(log_probs, along = 1)))
  return(apply(MARGIN = c(2,3,4), abind::abind(log_probs, along = 1), FUN = sumLogProbs))
}

sumLogProbs <- function(log_probs){
  max_log_probs <- max(log_probs)
  res <- log(sum(exp(log_probs-max_log_probs))) + max_log_probs
  res
}

#' @export
shapeParams <- function(param_vec, model){
  res <- list()
  res$emission_coeffs <- createEmissionCoeffs(model)
  res$transition_parameters <- createTransitionParameters(model)
  res$emission_coeffs[] <- param_vec[seq(length(res$emission_coeffs))]
  param_vec <- param_vec[-seq(length(res$emission_coeffs))]
  for(i in seq(length(res$transition_parameters))){
    res$transition_parameters[[i]][] <- param_vec[seq(length(res$transition_parameters[[i]]))]
    param_vec <- param_vec[-seq(length(res$transition_parameters[[i]]))]
  }
  return(res)
}

#' @export
flattenParams <- function(params){
  c(
    c(params[[grep("emission", names(params))]]),
    unlist(lapply(params[[grep("transition", names(params))]], c))
  )
}

# shamefully stolen from R package UniLindleyApprox
finiteDiffGrad <- function (fun, theta, eps = 1e-06, ...){
  n <- length(theta)
  gradient <- numeric(n)
  for (i in 1:n) {
    theta_plus <- theta
    theta_minus <- theta
    theta_plus[i] <- theta_plus[i] + eps
    theta_minus[i] <- theta_minus[i] - eps
    f_plus <- fun(theta_plus, ...)
    f_minus <- fun(theta_minus, ...)
    gradient[i] <- (f_plus - f_minus)/(2 * eps)
  }
  gradient
}

sum_lists <- function(lsts) {
  if (!is.list(lsts) || length(lsts) == 0) {
    stop("Provide a non-empty list of lists")
  }
  is_l <- vapply(lsts, is.list, logical(1))

  if (all(is_l)) {
    n <- unique(lengths(lsts))
    if (length(n) != 1) stop("All lists must have the same length")
    res <- lapply(seq_len(n), function(i) {
      sum_lists(lapply(lsts, `[[`, i))      # gather i-th element of each list, recurse
    })
    names(res) <- names(lsts[[1]])
    res
  } else if (any(is_l)) {
    stop("Structure mismatch: some elements are lists and others are not")
  } else {
    Reduce(`+`, lsts)                       # leaves: numbers, vectors, matrices, arrays
  }
}
