forwardR <- function(emission_model, transition_model, data_seq,
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

  # Emission parameters depending on explanatory variables
  emission_parameters <-
    emissionParameters(
      emission_regression_coefficients = emission_regression_coefficients,
      explanatory_variables = data_seq$explanatory_variables[,emission_model$emission_explanatory_variables,drop=F])
  # looping over observations by chronological order
  for(time_idx in seq(n_time)){
    # 1 Updating current knowledge using the current emission log likelihood #####
    for(latent_state in transition_model$latent_states){
      # 1a evaluation of the likelihood of the emission for each latent state ####
      filtering_log_probs[[latent_state]] <-
        filtering_log_probs[[latent_state]] +
        emission_model$emission_log_likelihood(
          emission = data_seq$emissions[[time_idx]],
          emission_parameters_vector = emission_parameters[[latent_state]][time_idx,])
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

forwardCpp <- function(emission_model, transition_model, data_seq,
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
    for(latent_state in transition_model$latent_states){
      # 1 evaluation of the likelihood of the emission for each latent state ####
      filtering_log_probs[[latent_state]] <-
        filtering_log_probs[[latent_state]] +
        emission_model$emission_log_likelihood(
          emission = data_seq$emissions[[time_idx]],
          emission_parameters_vector =
            abind::adrop(
              data_seq$explanatory_variables[time_idx, emission_model$emission_explanatory_variables] %*%
                abind::adrop(emission_regression_coefficients[,,latent_state,drop=F],3), 1)
        )
    }
    # 2 saving filtering probability when whole history of log-probabilities is needed ####
    for(latent_state in transition_model$latent_states){
      if(return_logprob_history)logprob_history[[latent_state]][,time_idx] <-
          filtering_log_probs[[latent_state]]
    }
    # 2 transferring the information to next time step using transition rules  #####
    if(time_idx < n_time){
      # initializing outflow
      out_logprobs <- lapply(transition_model$misc$number_outstates, function(x){
        rep(-Inf, x)
      })
      for(latent_state in transition_model$latent_states)names(out_logprobs[[latent_state]]) <-
          transition_model$misc$outstates[[latent_state]]
      # 2a updating the probabilities using the transition rules ####
      for(latent_state in transition_model$misc$non_absorbing_states) {
        value_at_btf_knots <- valueAtBtfKnots(
          explanatory_variable = data_seq$explanatory_variables[time_idx, ],
          transition_param = transition_parameters[[latent_state]]
        )
        depth <- transition_model$misc$depth[latent_state]
        btf_knots <- transition_model$BTF_per_state[[latent_state]]
        btf_multiplicators <- if (depth > 1) {
          exp(-matrix(apply(log(value_at_btf_knots), 2, diff),
                      nrow = nrow(value_at_btf_knots) - 1) / diff(btf_knots))
        } else {
          matrix(0, 0, ncol(value_at_btf_knots))
        }
        transition(
          filtering_log_probs[[latent_state]],   # modified in place
          out_logprobs[[latent_state]],          # modified in place
          value_at_btf_knots, btf_multiplicators, btf_knots, depth
        )
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
          if(depth>2){filtering_log_probs[[latent_state]][2:(depth - 1)] <-
            filtering_log_probs[[latent_state]][1:(depth - 2)]}
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
  if(return_logprob_history)return(list(filtering_log_probs = filtering_log_probs,
                                        logprob_history = logprob_history))
}

endIdx <- function(time_idx, depth)(-(time_idx)) %% depth +1

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

initializeEmissionDiffLogprobs <-
  function(filtering_log_probs, transition_model, emission_regression_coefficients){
    emission_diff <- lapply(
      filtering_log_probs, function(x)array(x, dim = c(length(x), dim(emission_regression_coefficients))))
    for(latent_state in transition_model$latent_states){
      dimnames(emission_diff[[latent_state]])[[1]] <-
        paste("lag", seq(length(filtering_log_probs[[latent_state]])), sep = "")
      dimnames(emission_diff[[latent_state]])[c(2,3,4)] <-
        dimnames(emission_regression_coefficients)[c(1,2,3)]
    }
    emission_diff
  }
initializeTransitionDiffLogprobs <-
  function(filtering_log_probs, transition_model, transition_parameters){
    res <- lapply(
      filtering_log_probs, function(filtering_log_probs){
        lapply(transition_parameters, function(transition_parameter){
          res <- array(
            filtering_log_probs[1],
            dim = c(length(filtering_log_probs), dim(transition_parameter)),
            dimnames = c(list(names(filtering_log_probs)), dimnames(transition_parameter)))
        })
      })
    res[which(sapply(res, length)==0)]<- NULL
    res
  }
initializeLogprobs <-
  function(transition_model, emission_regression_coefficients,
           transition_parameters){
    # initializing log-probabilities
    filtering_log_probs <- lapply(
      transition_model$misc$depth,
      function(x){
        res <- log(rep(1/(x*model$transition_model$n_latent_states), x))
        names(res) <- paste("lag", seq(length(res)))
        res
      })
    return(list(
      base = filtering_log_probs,
      emission_f = initializeEmissionDiffLogprobs(filtering_log_probs, transition_model, emission_regression_coefficients),
      emission_b = initializeEmissionDiffLogprobs(filtering_log_probs, transition_model, emission_regression_coefficients),
      transition_f = initializeTransitionDiffLogprobs(filtering_log_probs, transition_model, transition_parameters),
      transition_b = initializeTransitionDiffLogprobs(filtering_log_probs, transition_model, transition_parameters)
    ))
  }

initializeOutLogProbsEmission <- function(transition_model, emission_regression_coefficients){
  lapply(transition_model$misc$outstates, function(x){
    res <- array(-Inf, c(length(x), dim(emission_regression_coefficients)))
    dimnames(res)[[1]] <- x
    res
  })}
initializeOutLogProbsTransition <- function(transition_model, transition_parameters){
  res1 <- lapply(transition_model$misc$outstates, function(outstates){
    res2 <- lapply(transition_parameters, function(transition_param){
      res3 <- array(-Inf, dim = c(length(outstates), dim(transition_param)),
                    dimnames = c(list(outstates), dimnames(transition_param)))
    })
  })
}
initializeOutLogProbs <- function(transition_model, emission_regression_coefficients, transition_parameters){
  # anything smarter causes mem issues
  return(list(
    base = lapply(transition_model$misc$outstates, function(x){
      sapply(x, function(x)-Inf)
    }),
    emission_f = initializeOutLogProbsEmission(transition_model, emission_regression_coefficients),
    emission_b = initializeOutLogProbsEmission(transition_model, emission_regression_coefficients),
    transition_f = initializeOutLogProbsTransition(transition_model, transition_parameters),
    transition_b = initializeOutLogProbsTransition(transition_model, transition_parameters)
  ))
}

# transpose list names
transpose_list <- function(l) {
  inner_names <- names(l[[1]])
  setNames(
    lapply(inner_names, function(n) lapply(l, `[[`, n)),
    inner_names)
}

# collapse log-probs into ll
collapseLogProbs <-  function(log_probs){
  if(is.list(log_probs[[1]]))return(lapply(log_probs, collapseLogProbs))
  if(is.null(dim(log_probs[[1]])))return(sumLogProbs(abind::abind(log_probs, along = 1)))
  return(apply(MARGIN = c(2,3,4), abind::abind(log_probs, along = 1), FUN = sumLogProbs))
}

# emission and transition grad
paramLLGrad <- function(emission_model, transition_model, data_seq,
                        emission_regression_coefficients, transition_parameters){
  n_time <- length(data_seq$emissions)
  # initializing log probabilities
  log_probs <- initializeLogprobs(transition_model,
                                  emission_regression_coefficients, transition_parameters)
  # finite diff constant
  finite_diff_constant <- 1e-6
  # looping over observations by chronological order
  for(time_idx in seq(n_time)){
    # 1 Updating current knowledge using the current emission log likelihood #####
    for(latent_state in transition_model$latent_states){
      # base emission log likelihood
      emission_parameters_vector =
        abind::adrop(
          data_seq$explanatory_variables[time_idx, emission_model$emission_explanatory_variables] %*%
            abind::adrop(emission_regression_coefficients[,,latent_state,drop=F],3), 1)
      emission_log_likelihood <-
        emission_model$emission_log_likelihood(
          emission = data_seq$emissions[[time_idx]],
          emission_parameters_vector = emission_parameters_vector
        )
      # incrementing in base likelihood evaluation
      log_probs$base[[latent_state]] <-
        # incrementing in difference with respect to transition parameters
        log_probs$base[[latent_state]] + emission_log_likelihood
      for(latent_state_2 in transition_model$misc$non_absorbing_states) {
        log_probs$transition_f[[latent_state]][[latent_state_2]] <-
          log_probs$transition_f[[latent_state]][[latent_state_2]] + emission_log_likelihood
        log_probs$transition_b[[latent_state]][[latent_state_2]] <-
          log_probs$transition_b[[latent_state]][[latent_state_2]] + emission_log_likelihood
      }
      # incrementing in difference with respect to emission parameters
      log_probs$emission_f[[latent_state]][] <-
        log_probs$emission_f[[latent_state]][] +
        emission_log_likelihood
      log_probs$emission_b[[latent_state]][] <-
        log_probs$emission_b[[latent_state]][] +
        emission_log_likelihood
      # supplementary increment, just for difference with respect to emission parameters
      d_lik_d_emission_param <-
        finite_diff_constant *
        outer(
          data_seq$explanatory_variables[time_idx, emission_model$emission_explanatory_variables],
          emission_model$emission_log_likelihood_grad(
            emission = data_seq$emissions[[time_idx]],
            emission_parameters_vector = emission_parameters_vector)
        )
      log_probs$emission_f[[latent_state]][,,,latent_state] <-
        log_probs$emission_f[[latent_state]][,,,latent_state] +
        rep(d_lik_d_emission_param, each = transition_model$misc$depth[[latent_state]])
      log_probs$emission_b[[latent_state]][,,,latent_state] <-
        log_probs$emission_b[[latent_state]][,,,latent_state] -
        rep(d_lik_d_emission_param, each = transition_model$misc$depth[[latent_state]])
    }
    # 2 transferring the information to next time step using transition rules  #####
    if(time_idx < n_time){
      # initializing outflow
      out_log_probs <- initializeOutLogProbs(
        transition_model = transition_model,
        emission_regression_coefficients = emission_regression_coefficients,
        transition_parameters = transition_parameters)
      # 2a updating the probabilities using the transition rules ####
      for(latent_state in transition_model$misc$non_absorbing_states) {
        depth <- transition_model$misc$depth[latent_state]
        end_idx <- endIdx(time_idx, depth)
        btf_knots <- transition_model$BTF_per_state[[latent_state]]
        transition_several(
          # base
          filtering_log_probs = log_probs$base[[latent_state]],
          out_logprobs = out_log_probs$base[[latent_state]],
          # difference wrt emission
          filtering_log_probs_emission_diff_f = log_probs$emission_f[[latent_state]],
          out_logprobs_emission_diff_f =  out_log_probs$emission_f[[latent_state]],
          filtering_log_probs_emission_diff_b = log_probs$emission_b[[latent_state]],
          out_logprobs_emission_diff_b =  out_log_probs$emission_b[[latent_state]],
          # difference wrt transition
          filtering_log_probs_transition_diff_f = log_probs$transition_f[[latent_state]],
          out_logprobs_transition_diff_f =  out_log_probs$transition_f[[latent_state]],
          filtering_log_probs_transition_diff_b = log_probs$transition_b[[latent_state]],
          out_logprobs_transition_diff_b =  out_log_probs$transition_b[[latent_state]],
          # rest
          transition_param     = transition_parameters[[latent_state]],
          explanatory_variable = data_seq$explanatory_variables[time_idx, ],
          btf_knots            = as.integer(btf_knots),
          depth = depth, last_idx = end_idx
        )
      }
       # 2b translating probabilities along time spent ####
       for(latent_state in transition_model$latent_states){
         depth <- transition_model$misc$depth[latent_state]
         # excluding absorbing and simple Markov states
         if(depth >1){
           end_idx <- endIdx(time_idx, depth)
           next_end_idx <- endIdx(time_idx + 1, depth)
           # adding penultimate probability to last probability
           # base
           log_probs$base[[latent_state]][next_end_idx] <-
             addProbs(
               log_probs$base[[latent_state]][next_end_idx],
               log_probs$base[[latent_state]][end_idx])
           # emission
           log_probs$emission_f[[latent_state]][next_end_idx,,,] <-
             addProbs(
               log_probs$emission_f[[latent_state]][next_end_idx,,,],
               log_probs$emission_f[[latent_state]][end_idx,,,])
           log_probs$emission_b[[latent_state]][next_end_idx,,,] <-
             addProbs(
               log_probs$emission_b[[latent_state]][next_end_idx,,,],
               log_probs$emission_b[[latent_state]][end_idx,,,])
           # transition
           for(latent_state_2 in transition_model$misc$non_absorbing_states){
             log_probs$transition_f[[latent_state]][[latent_state_2]][next_end_idx,,,] <-
               addProbs(
                 log_probs$transition_f[[latent_state]][[latent_state_2]][next_end_idx,,,],
                 log_probs$transition_f[[latent_state]][[latent_state_2]][end_idx,,,])
             log_probs$transition_b[[latent_state]][[latent_state_2]][next_end_idx,,,] <-
               addProbs(
                 log_probs$transition_b[[latent_state]][[latent_state_2]][next_end_idx,,,],
                 log_probs$transition_b[[latent_state]][[latent_state_2]][end_idx,,,])
           }
           # reinitializing (not first) end idx probability
           # base
           log_probs$base[[latent_state]][end_idx] <- -Inf
           # emission
           log_probs$emission_f[[latent_state]][end_idx,,,] <- -Inf
           log_probs$emission_b[[latent_state]][end_idx,,,] <- -Inf
           # transition
           for(latent_state_2 in transition_model$misc$non_absorbing_states){
            log_probs$transition_f[[latent_state]][[latent_state_2]][end_idx,,,] <- -Inf
            log_probs$transition_b[[latent_state]][[latent_state_2]][end_idx,,,] <- -Inf
           }
         }
       }
      # 2c transferring outflow to time spent = 1 ####
      for(origin_state in transition_model$misc$non_absorbing_states){
        for(exit_state in transition_model$misc$outstates[[origin_state]]){
          end_idx <- endIdx(time_idx, transition_model$misc$depth[[exit_state]])
          log_probs$base[[exit_state]][end_idx] <-
            addProbs(
              log_probs$base[[exit_state]][end_idx],
              out_log_probs$base[[origin_state]][exit_state]
            )
          log_probs$emission_f[[exit_state]][end_idx,,,] <-
            addProbs(
              log_probs$emission_f[[exit_state]][end_idx,,,],
              out_log_probs$emission_f[[origin_state]][exit_state,,,]
            )
          log_probs$emission_b[[exit_state]][end_idx,,,] <-
            addProbs(
              log_probs$emission_b[[exit_state]][end_idx,,,],
              out_log_probs$emission_b[[origin_state]][exit_state,,,]
            )
          for(diff_state in transition_model$misc$non_absorbing_states){
            log_probs$transition_f[[exit_state]][[diff_state]][end_idx,,,] <-
              addProbs(
                log_probs$transition_f[[exit_state]][[diff_state]][end_idx,,,],
                out_log_probs$transition_f[[origin_state]][[diff_state]][exit_state,,,]
              )
            log_probs$transition_b[[exit_state]][[diff_state]][end_idx,,,] <-
              addProbs(
                log_probs$transition_b[[exit_state]][[diff_state]][end_idx,,,],
                out_log_probs$transition_b[[origin_state]][[diff_state]][exit_state,,,]
              )
          }
        }
      }
    }
  }
  log_probs$transition_f <- transpose_list(log_probs$transition_f)
  log_probs$transition_b <- transpose_list(log_probs$transition_b)
  log_probs <- collapseLogProbs(log_probs)
  res <- list()
  res$base <- log_probs$base
  res$grad_emission <- (
    log_probs$emission_f - log_probs$emission_b
  )/(2*finite_diff_constant)
  res$diff_transition_f <-
    log_probs$transition_f
  res$diff_transition_b <-
    log_probs$transition_b
  return(res)
}

sumLogProbs <- function(log_probs){
  max_log_probs <- max(log_probs)
  res <- log(sum(exp(log_probs-max_log_probs))) + max_log_probs
  res
}

paramLL <- function(emission_model, transition_model, data_seq,
                    emission_regression_coefficients, transition_parameters){
  res <- unlist(forwardCpp(emission_model, transition_model, data_seq,
                           emission_regression_coefficients, transition_parameters,
                           return_logprob_history = FALSE))
  sumLogProbs(res)
}
