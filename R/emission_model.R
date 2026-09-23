
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


