remove(list = ls())# I dont care if you judge me
set.seed(1)
source("R/initialize_model.R")
source("R/check_data.R")
source("R/transition_model.R")
source("R/emission_model.R")
source("R/utils.R")
source("R/simulate_data.R")
source("R/plot.R")
source("R/forward.R")

# list of data without proper emissions ####
seq_length = 2000
temperature_series = 20 + 4 * GpGp::fast_Gp_sim(covparms = c(1,10,1.5, 0), locs = cbind(seq(seq_length), 1), m = 5) + 3 * rnorm(seq_length) + 12 * sin(seq(seq_length)*2*pi/365)
individual <- list(
  explanatory_variables = matrix(cbind(temperature_series), ncol= 1, dimnames = list(NULL, c("day_temperature")) ),
  emissions = lapply(seq(seq_length), function(x)return(NA))
)

plot(temperature_series)
data_list = list(
  individual_1 = individual,
  individual_2 = individual,
  individual_3 = individual,
  individual_4 = individual,
  individual_4 = individual,
  individual_5 = individual,
  individual_6 = individual
)
plot(data_list$individual_1$explanatory_variables[,1])

# beginning model initialization without proper emission ####

# First step of model: providing data and latent states names ####
model = initializeStep1(
  # names of the latent states
  latent_states = c("S", "I", "R", "D"),
  # observations
  data_list = data_list
)

# Second step: specifying transition graph between latent states, and BTFs for semi-Markov ####
# Modifying possible transitions
model$transition_model$to_specify$possible_transitions["S","R"]=FALSE
model$transition_model$to_specify$possible_transitions["I","S"]=FALSE
model$transition_model$to_specify$possible_transitions["R","I"]=FALSE
model$transition_model$to_specify$possible_transitions["D",c("S", "I", "R")]=FALSE
plotTransitionGraph(model)
# BTF per state
print(model$transition_model$to_specify$BTF_per_state)
model$transition_model$to_specify$BTF_per_state$I = makeBTF(c(20, 40, 60))
model$transition_model$to_specify$BTF_per_state$R = makeBTF(c(150, 300, 450))
model = initializeStep2(model)

# Third step: specifying explanatory variables effects on transition parameters ####
model$transition_model$to_specify$explanatory_variables_effects
model$transition_model$to_specify$explanatory_variables_effects$S[2] = T
model = initializeStep3(model)

# Fourth step: intializing emission model with the emission parameter names and the explanatory variables ####
model$emission_model$to_specify$emission_parameters_names <- c(
  "pcr_logprob",
  "sero_logprob",
  "body_temp_mean",
  "body_temp_logsd",
  "death_obs_logprob")
model$emission_model$to_specify$emission_explanatory_variables
model = initializeStep4(model)

# (Secret step 1/2) Sampling (hidden) latent states ####
transition_parameters = createTransitionParameters(model = model)

transition_parameters$S[,"Intercept","I"] <- -4
transition_parameters$S[,"Intercept","D"] <- -10
transition_parameters$S[,"day_temperature","I"] <- -.2
transition_parameters$S[,"day_temperature","D"] <- 0

transition_parameters$I[,"Intercept","R"] <- c(-10,-6,0,3)
transition_parameters$I[,"Intercept","D"] <- c(-4,-4,-10,-10)

transition_parameters$R[,"Intercept","S"] <- c(-6,-5,-5,-4)
transition_parameters$R[,"Intercept","D"] <- c(-10,-10,-10,-10)

model <- sampleLatentState(model = model, transition_parameters = transition_parameters, initial_state = "S")
plotLatentState(model, 1)
plotLatentState(model, 1, F)
plotLatentState(model, 2)
plotLatentState(model, 2, F)
plotLatentState(model, 3)
plotLatentState(model, 3, F)
plotLatentState(model, 4)
plotLatentState(model, 4, F)

# (Secret step 2/2) Sampling emissions from the latent states ####
emission_regression_coefficients = createRandomEmissionCoeffs(
  emission_parameters = model$emission_model$dont_touch$emission_parameters_names,
  explanatory_variables = model$emission_model$dont_touch$emission_explanatory_variables,
  latent_states = model$transition_model$latent_states)

emission_regression_coefficients["Intercept","pcr_logprob",       "S"] = -10
emission_regression_coefficients["Intercept","sero_logprob",      "S"] = -10
emission_regression_coefficients["Intercept","body_temp_mean",    "S"] = 37
emission_regression_coefficients["Intercept","body_temp_logsd",   "S"] = -3
emission_regression_coefficients["Intercept","death_obs_logprob", "S"] = -100

emission_regression_coefficients["Intercept","pcr_logprob",       "R"] = emission_regression_coefficients["Intercept","pcr_logprob",    "S"]
emission_regression_coefficients["Intercept","sero_logprob",      "R"] = 10
emission_regression_coefficients["Intercept","body_temp_mean",    "R"] = emission_regression_coefficients["Intercept","body_temp_mean", "S"]
emission_regression_coefficients["Intercept","body_temp_logsd",   "R"] = emission_regression_coefficients["Intercept","body_temp_logsd","S"]
emission_regression_coefficients["Intercept","death_obs_logprob", "R"] = emission_regression_coefficients["Intercept","death_obs_logprob",  "S"]

emission_regression_coefficients["Intercept","pcr_logprob",       "I"] = 10
emission_regression_coefficients["Intercept","sero_logprob",      "I"] = 10
emission_regression_coefficients["Intercept","body_temp_mean",    "I"] = 39
emission_regression_coefficients["Intercept","body_temp_logsd",   "I"] = -2
emission_regression_coefficients["Intercept","death_obs_logprob", "I"] = emission_regression_coefficients["Intercept","death_obs_logprob",  "S"]

emission_regression_coefficients["Intercept","pcr_logprob",       "D"] = 0
emission_regression_coefficients["Intercept","sero_logprob",      "D"] = 0
emission_regression_coefficients["Intercept","body_temp_mean",    "D"] = 0
emission_regression_coefficients["Intercept","body_temp_logsd",   "D"] = 0
emission_regression_coefficients["Intercept","death_obs_logprob", "D"] = 100

emissionParameters(emission_regression_coefficients, model$data_list$individual_1$explanatory_variables[100,])

sample_one_emission <- function(emission_regression_coefficients, latent_state, explanatory_variable){
  emission_parameters <- explanatory_variable[dimnames(emission_regression_coefficients)[[1]]] %*% emission_regression_coefficients[,,latent_state]
  colnames(emission_parameters) <- dimnames(emission_regression_coefficients)[[2]]
  if(latent_state =="D")return(list("death"= T, "sero"= NA, "pcr"= NA, "body_temp" = NA))
  res <- list("death"= F, "sero"= NA, "pcr"= NA, "body_temp" = NA)
  if(runif(1)>.98){
    res$sero <- rbinom(1,1,softmax(emission_parameters[,"sero_logprob"])[2])
    res$pcr <-  rbinom(1,1,softmax(emission_parameters[,"pcr_logprob"])[2])
  }
  if(runif(1)>0){
    res$body_temp <- rnorm(1, emission_parameters[,"body_temp_mean"], exp(emission_parameters[,"body_temp_logsd"]))
  }
  return(res)
}

model <- sampleEmissions(model, emission_regression_coefficients, sample_one_emission)
dev.off()
plot(as.numeric(as.factor(sapply(model$data_list$individual_1$emissions, function(x)x[["death"]]))))
plot(as.numeric(as.factor(sapply(model$data_list$individual_4$emissions, function(x)x[["death"]]))))
plot(as.numeric(sapply(model$data_list$individual_1$emissions, function(x)x[["body_temp"]])))
# plot(as.numeric(sapply(model$data_list$individual_1$emissions, function(x)x[["pcr"]])))
# plot(as.numeric(sapply(model$data_list$individual_1$emissions, function(x)x[["sero"]])))

# Fifth step: specifying log-prior and log-likelihood for the emission parameters ####
#emission_regression_coefficients <- model$emission_model$to_specify$regression_coefficients_array_NOFILL

model$emission_model$to_specify$emission_regression_coefficients_log_prior <- function(emission_regression_coefficients){
  emission_regression_coefficients_mean <- 0* model$emission_model$to_specify$regression_coefficients_array_NOFILL
  emission_regression_coefficients_sd   <- 0* model$emission_model$to_specify$regression_coefficients_array_NOFILL
  emission_regression_coefficients_mean[,"pcr_logprob",] <- c(-10, 10,  -10,  0)
  emission_regression_coefficients_sd[,"pcr_logprob",] <-   c(.01, .01, .01, .01)
  emission_regression_coefficients_mean[,"sero_logprob",] <-   c(-10,  10,  10,  0)
  emission_regression_coefficients_sd[,  "sero_logprob",] <-   c(.01, .01, .01, .01)
  emission_regression_coefficients_mean[,"body_temp_mean",] <-   c(37, 39, 37, 0)
  emission_regression_coefficients_sd[,  "body_temp_mean",] <-   c(.1, .5, .1, .01)
  emission_regression_coefficients_mean[,"body_temp_logsd",] <-   c(-1, -1, -1, -100)
  emission_regression_coefficients_sd[,  "body_temp_logsd",] <-   c( 1,  1,  1,  .01)
  emission_regression_coefficients_mean[,"death_obs_logprob",] <-   c(-100, -100, -100, 100)
  emission_regression_coefficients_sd[,  "death_obs_logprob",] <-   c(.01, .01, .01, .01)
  sum(dnorm(c(emission_regression_coefficients), mean =  c(emission_regression_coefficients_mean),
        sd = c(emission_regression_coefficients_sd), log = T))
}

model$emission_model$to_specify$emission_log_likelihood = function(
    emission,
    death_obs_logprob,
    pcr_logprob,
    sero_logprob,
    body_temp_mean,
    body_temp_logsd
){
  res <- 0
  if(!is.na(emission$death))res <- res +  dbinom(x = emission$death, size = 1, prob = exp(death_obs_logprob)/(1+exp(death_obs_logprob)),log = T)
  if(!is.na(emission$pcr))res <- res +  dbinom(x = emission$pcr, size = 1, prob = exp(pcr_logprob)/(1+exp(pcr_logprob)),log = T)
  if(!is.na(emission$sero))res <- res + dbinom(x = emission$sero, size = 1, prob = exp(sero_logprob)/(1+exp(sero_logprob)),log = T)
  if(!is.na(emission$body_temp))res <- res + dnorm(emission$body_temp, body_temp_mean, exp(body_temp_logsd),log = T)
  res
}


model = initializeStep5(model)





emission_model <- model$emission_model
transition_model <- model$transition_model
data_seq <- model$data_list$individual_1

time_idx = 1
latent_state = "S"
return_logprob_history = T

source("R/forward.R")
test <- forward(emission_model = model$emission_model, transition_model = model$transition_model,
                data_seq = model$data_list$individual_1,
                emission_regression_coefficients = emission_regression_coefficients,
                transition_parameters = transition_parameters, return_logprob_history = T)

dev.off()
filtering_probs <- filteringProbs(logprob_history = test$logprob_history)
plot(col(filtering_probs),
     filtering_probs + .0025*row(filtering_probs),
     col = row(filtering_probs), pch = ".", cex = 2,
     xlab = "time", ylab = "filtering probabilities")
legend("topright", fill = seq(nrow(filtering_probs)), legend = row.names(filtering_probs))

