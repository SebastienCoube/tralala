
forward <- function(emission_model, transition_model, data_seq,
                    emission_regression_coefficients, transition_parameters,
                    return_logprob_history = FALSE){
  n_time <- length(data_seq$emissions)
  # initializing log-probabilities
  filtering_log_probs <- lapply(
    transition_model$misc$depth,
    function(x)log(rep(1/(x*model$transition_model$n_latent_states), x)))
  # checking that initial probabilities sum to 1
  # sum(exp(unlist(filtering_log_probs)))

  # case when whole history of log-probabilities is needed
  if(return_logprob_history)logprob_history <-
      lapply(filtering_log_probs, function(x)matrix(0, length(x), n_time))

  # looping over observations by chronological order
  for(time_idx in seq(n_time)){
    # 1 Updating current knowledge using the current emission log likelihood #####
    # emission parameters depending on explanatory variables
    emission_parameters <-
      emissionParameters(
        emission_regression_coefficients = emission_regression_coefficients,
        explanatory_variables = data_seq$explanatory_variables[time_idx,emission_model$emission_explanatory_variables])
    for(latent_state in transition_model$latent_states){
      # 1a evaluation of the likelihood of the emission for each latent state ####
      filtering_log_probs[[latent_state]] <-
        filtering_log_probs[[latent_state]] +
        do.call(
          emission_model$emission_log_likelihood,
          c(list("emission" = data_seq$emissions[[time_idx]]), as.list(emission_parameters[,latent_state]))
        )
      # 1b saving filtering probability when whole history of log-probabilities is needed ####
      if(return_logprob_history)logprob_history[[latent_state]][,time_idx] <-
          filtering_log_probs[[latent_state]]
    }
    # 2 transferring the information to next time step using transition rules  #####
    if(time_idx < n_time){
      # initializing outflow
      out_logprobs <- lapply(transition_model$misc$number_outstates, function(x)rep(-Inf, x))
      # 2a updating the probabilities using the transition rules ####
      for(latent_state in transition_model$misc$non_absorbing_states){
        # value of probabilities at BTF knots
        value_at_btf_knots <- valueAtBtfKnots(
          explanatory_variable = data_seq$explanatory_variables[time_idx,],
          transition_param =  transition_parameters[[latent_state]]
        )
        # initialization of transition probabilities
        tr_prob <- value_at_btf_knots[1,]
        m_log_tr_prob <- tr_prob
        # multiplicator between BTF knots
        if(transition_model$misc$depth[latent_state]>1){
          btf_multiplicators <- exp(
            -matrix(
              apply(log(value_at_btf_knots), 2, diff),
              nrow = nrow(value_at_btf_knots) - 1)/
              diff(transition_model$BTF_per_state[[latent_state]])
          )
        }
        # checking that BTF multiplicators allow to obtain next BTF knot
        # value_at_btf_knots[-nrow(value_at_btf_knots),]- # all BTF knots except the last
        #   btf_multiplicators^diff(transition_model$BTF_per_state[[latent_state]])*
        #   value_at_btf_knots[-1,] # all BTF knots except the first
        # checking coherence of transition probabilities obtained through multiplicators
        # trprob_rec <-matrix(0, transition_model$misc$depth[latent_state], length(tr_prob))
        for(time_spent in seq(transition_model$misc$depth[latent_state], 1)){
          # computing transition probability
          knot_match <- match(time_spent, transition_model$BTF_per_state[[latent_state]])
          if(!is.na(knot_match)){
            # if time spent is precisely at knot, fetch exact transition probability
            # and btf multiplicator value
            # checking that last transition prob obtained by multiplication lands on next knot
            # print(
            #   (value_at_btf_knots[knot_match,]-tr_prob*btf_mult)/value_at_btf_knots[knot_match,])
            tr_prob[] <- value_at_btf_knots[knot_match,]
            if(time_spent > 1){btf_mult <- btf_multiplicators[knot_match-1,]}
          }else{
            # if time spent is between knots, multiply and renormalize
            tr_prob[] <- normalize(tr_prob * btf_mult)
          }
          # checking consistence of transition probabilities obtained through multiplicators
          # trprob_rec[time_spent,] <- tr_prob
          # plot(trprob_rec[,1]); points(model$transition_model$BTF_per_state[[latent_state]], value_at_btf_knots[,1], col=2, pch = 16)
          # plot(trprob_rec[,2]); points(model$transition_model$BTF_per_state[[latent_state]], value_at_btf_knots[,2], col=2, pch = 16)
          # plot(trprob_rec[,3]); points(model$transition_model$BTF_per_state[[latent_state]], value_at_btf_knots[,3], col=2, pch = 16)

          # multiplying log(tr_prob) by the probability of
          m_log_tr_prob[] <- filtering_log_probs[[latent_state]][time_spent] + log(tr_prob)
          # incrementing outflow probability
          out_logprobs[[latent_state]] <- addProbs(
            out_logprobs[[latent_state]], m_log_tr_prob[-1])
          # in-place modification of remaining probabilities
          filtering_log_probs[[latent_state]][time_spent] <- m_log_tr_prob[1]
        }
      }
      # 2b translating probabilities along time spent ####
      for(latent_state in transition_model$latent_states){
        depth <- transition_model$misc$depth[latent_state]
        # excluding absorbing and simple Markov states
        if(depth >1){
          # adding penultimate probability to last probability
          filtering_log_probs[[latent_state]][depth] <-
            addProbs(
              filtering_log_probs[[latent_state]][depth],
              filtering_log_probs[[latent_state]][depth-1])
          # translating probabilities along time spent
          if(depth>2){
            for(time_spent in seq(depth-1, 2))
              filtering_log_probs[[latent_state]][time_spent] <-
                filtering_log_probs[[latent_state]][time_spent-1]
          }
          # reinitializing first probability
          filtering_log_probs[[latent_state]][1] <- -Inf
        }
      }
      # 2c transferring outflow to time spent = 1 ####
      for(latent_state in transition_model$misc$non_absorbing_states){
        for(exit_state in transition_model$misc$outstates[[latent_state]]){
          filtering_log_probs[[exit_state]][1] <-
            addProbs(
              filtering_log_probs[[exit_state]][1],
              out_logprobs[[latent_state]][exit_state]
            )
        }
      }
    }
  }
  if(!return_logprob_history)return(list(filtering_log_probs = filtering_log_probs))
  if(return_logprob_history)return(list(filtering_log_probs = filtering_log_probs, logprob_history = logprob_history))
}

filteringProbs <- function(logprob_history){
  probs <- t(do.call(rbind, logprob_history))
  probs <- probs - apply(probs, 1, max)
  probs <- exp(probs)
  M <- matrix(0, sum(sapply(logprob_history, nrow)), length(sapply(logprob_history, nrow)))
  M[cbind(seq(sum(sapply(logprob_history, nrow))), rep(seq(length(sapply(logprob_history, nrow))), sapply(logprob_history, nrow)))] <- 1
  colnames(M) <- names(logprob_history)
  probs <- probs %*%M
  probs <- (apply(probs, 1, normalize))
  probs
}
