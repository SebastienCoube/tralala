value_at_btf_knots = function(explanatory_variable, transition_param){
  t(apply(
    apply(transition_param, c(1,3), function(x)sum(x*explanatory_variable[dimnames(transition_param)[[2]]])),
    1, softmax))
}

softmax = function(x){
  res = c(1, exp(x))
  res = res/sum(res)
  res
}

normalize = function(x){x / sum(x)}

emission_parameters <- function(emission_regression_coefficients, explanatory_variables){
  res <- apply(
    emission_regression_coefficients, 3,
    function(x)explanatory_variables[dimnames(emission_regression_coefficients)[[1]]] %*% x)
  row.names(res) <- dimnames(emission_regression_coefficients)[[2]]
  res
}
