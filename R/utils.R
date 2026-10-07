valueAtBtfKnots = function(explanatory_variable, transition_param){
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

