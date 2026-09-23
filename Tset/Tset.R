remove(list = ls())# I dont care if you judge me
source("R/initialize_model.R")
source("R/check_data.R")
source("R/transition_model.R")
source("R/emission_model.R")
source("R/forward.R")

# list of data
seq_length = 2000
temperature_series = 20 + 4 * GpGp::fast_Gp_sim(covparms = c(1,10,1.5, 0), locs = cbind(seq(seq_length), 1), m = 5) + 3 * rnorm(seq_length) + 12 * sin(seq(seq_length)*2*pi/365)
plot(temperature_series)
data_list = list(
  individual_1 = list(
    explanatory_variables = matrix(cbind(temperature_series), ncol= 1, dimnames = list(NULL, c("day_temperature")) ),
    emissions = lapply(seq(seq_length), function(x)return(NA))
  ),
  individual_2 = list(
    explanatory_variables = matrix(cbind(temperature_series), ncol= 1, dimnames = list(NULL, c("day_temperature")) ),
    emissions = lapply(seq(seq_length), function(x)return(NA))
  )
)
plot(data_list$individual_1$explanatory_variables[,1])

# First step of model
model = initializeStep1(
  # names of the latent states
  latent_states = c("S", "I", "R", "D"),
  # observations
  data_list = data_list
)

# Second step of model
# Modifying possible transitions
model$transition_model$to_specify$possible_transitions["S","R"]=FALSE
model$transition_model$to_specify$possible_transitions["I","S"]=FALSE
model$transition_model$to_specify$possible_transitions["R","I"]=FALSE
model$transition_model$to_specify$possible_transitions["D",c("S", "I", "R")]=FALSE
print(model$transition_model$to_specify$possible_transitions)
plotTransitionGraph(model)

# Modifying variable basis interaction table
print(model$transition_model$to_specify$BTF_per_state)
model$transition_model$to_specify$BTF_per_state$I = makeBTF(c(5, 10, 15))
model$transition_model$to_specify$BTF_per_state$R = makeBTF(c(50, 100, 150))
model = initializeStep2(model)

model$transition_model$to_specify$explanatory_variables_effects

# Third step
model$transition_model$to_specify$explanatory_variables_effects$I[2,] = F
model$transition_model$to_specify$explanatory_variables_effects$R[2,] = F
model = initializeStep3(model)

# fourth step
model$emission_model$to_specify$emission_parameters_names <- c("pcr_logprob",
                                                               "sero_logprob",
                                                               "body_temp_mean",
                                                               "body_temp_logsd")
model$emission_model$to_specify$emission_explanatory_variables
model = initializeStep4(model)

emission_regression_coefficients_log_prior <- function(emission_regression_coeffs){
  emission_regression_coefficients_log_prior
}

model$emission_model$to_specify$regression_coefficients_array_NOFILL
model$emission_model$to_specify$emission_regression_coefficients_log_prior


model$emission_model$to_specify$emission_log_likelihood = function(
    emission,
    pcr_logprob,
    sero_logprob,
    body_temp_mean,
    body_temp_logsd
){
  res <- 0
  if(!is.NA(emission$pcr))res <- res + dbinom(res, 1, exp(pcr_logprob)/(1+exp(pcr_logprob)),log = T)
  if(!is.NA(emission$sero))res <- res + dbinom(res, 1, exp(sero_logprob)/(1+exp(sero_logprob)),log = T)
  if(!is.NA(emission$body_temp))res <- res + dnorm(res, body_temp_mean, exp(body_temp_logsd),log = T)
  res
}

model$transition_model$explanatory_variable_names
model = initializeStep4(model)

model$emission_model$to_specify$
model$emission_model$to_specify$active_explanatory_variables

model = initializeStep5(model)


# creating transition parameters with the format deduced from the transition model
params = createParams(model)
params$transition_params$S

# adding value for the transition parameters
params$transition_params$S$Intercept[,"I"]= 0
params$transition_params$S$Intercept[,"D"]= -8
params$transition_params$S$day_temperature[,"I"] = -1
params$transition_params$S$day_temperature[,"D"] = 0

params$transition_params$I$Intercept[1,"R"] = -4
params$transition_params$I$Intercept[2,"R"] = -3
params$transition_params$I$Intercept[3,"R"] = -2
params$transition_params$I$Intercept[4,"R"] = -1

params$transition_params$I$Intercept[1,"D"] = -8
params$transition_params$I$Intercept[2,"D"] = -5
params$transition_params$I$Intercept[3,"D"] = -4
params$transition_params$I$Intercept[4,"D"] = -3

params$transition_params$R$Intercept[1,"S"] = -8
params$transition_params$R$Intercept[2,"S"] = -8
params$transition_params$R$Intercept[3,"S"] = -7
params$transition_params$R$Intercept[4,"S"] = -6
params$transition_params$R$Intercept[,"D"] = -8

params$emission_params[,"S"] = c(37, -2)
params$emission_params[,"I"] = c(39, 0)
params$emission_params[,"R"] = c(37, -2)
params$emission_params[,"D"] = c(0, -10)



preprocessed_tbf = preprocessTbf(params, model)
data_seq = data_list[[1]]


