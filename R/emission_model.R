
createRandomEmissionCoeffs <- function(
    explanatory_variables, latent_states, emission_parameters){
  res <- array(
    0,
    dim = sapply(list(
      explanatory_variables,
      emission_parameters,
      latent_states
    ), length),
    dimnames = list(
      explanatory_variables,
      emission_parameters,
      latent_states
    ))
  res[] <- rnorm(length(res))
  res
}


# emission_log_likelihood <- function(emission, emission_parameters_vector)sum(emission*emission_parameters_vector)
# emission_log_likelihood_finite_diff <- emissionLogLikFiniteDiff(emission_log_likelihood)
# emission_log_likelihood_finite_diff(rnorm(100), rnorm(100))
getFiniteDiffGradEmissionLL <- function(emission_log_likelihood){
  finiteDiffGrad <- function(emission, emission_parameters_vector){
    eps <- 1e-6
    res <- list()
    res$eval <- emission_log_likelihood(emission, emission_parameters_vector)
    res$grad <- emission_parameters_vector
    emission_parameters_vector_f <- emission_parameters_vector
    emission_parameters_vector_b <- emission_parameters_vector
    for(i in seq(emission_parameters_vector)){
      emission_parameters_vector_f[i] <- emission_parameters_vector[i] + eps
      emission_parameters_vector_b[i] <- emission_parameters_vector[i] - eps
      res$grad[i] <- .5 * (
        emission_log_likelihood(emission, emission_parameters_vector_f) -
        emission_log_likelihood(emission, emission_parameters_vector_b)
        )/ eps
      emission_parameters_vector_f[i] <- emission_parameters_vector[i]
      emission_parameters_vector_b[i] <- emission_parameters_vector[i]
    }
    return(res)
  }
  return(finiteDiffGrad)
}

