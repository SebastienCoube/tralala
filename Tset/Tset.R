remove(list = ls())# I dont care if you judge me
source("R/initialize_model.R")
source("R/check_data.R")
source("R/transition_model.R")
source("R/emission_model.R")
source("R/forward.R")


# initializing transition model

# list of data
seq_length = 2000
temperature_series = 20 + 4 * GpGp::fast_Gp_sim(covparms = c(1,10,1.5, 0), locs = cbind(seq(seq_length-1), 1), m = 5) + 3 * rnorm(seq_length-1) + 12 * sin(seq(seq_length-1)*2*pi/365)
plot(temperature_series)
data_list = list(
  individual_1 = list(
    explanatory_variables_transition = matrix(cbind(1, temperature_series), ncol= 2, dimnames = list(NULL, c("Intercept", "day_temperature")) ),
    emissions = lapply(seq(seq_length), function(x)NA)
  ),
  individual_2 = list(
    explanatory_variables_transition = matrix(cbind(1, temperature_series), ncol= 2, dimnames = list(NULL, c("Intercept", "day_temperature")) ),
    emissions = lapply(seq(seq_length), function(x)NA)
  )
)

plot(data_list$individual_1$explanatory_variables_transition[,2])

# First step of model
model = initializeModelStep1(
  # names of the latent states
  latent_states = c("S", "I", "R", "D"),
  # observations
  data_list = data_list,
  # emission parameters whose value is fixed by the user
  fixed_emission_param_names = c("pcr_prob", "sero_prob", "death_prob"),
  # emission parameters whose value is estimated by the model
  estimated_emission_param_names = c("body_temp_mean", "log_body_temp_sd"),
  # log-likelihood of an emission
  emission_log_likelihood = function(
    estimated_emission_param_vec,
    fixed_emission_param_vec,
    emission,
    explanatory_variable_vec){
    if(!identical(NA, emission)){
      res=  0
      # PCR test
      if(!is.na(emission["PCR"])){
        res = res + dbinom(
          x = emission["PCR"], size = 1,
          prob = fixed_emission_param_vec["pcr_prob"],
          log = T
        )
      }
      # serologic test test
      if(!is.na(emission["sero"])){
        res = res + dbinom(
          x = emission["sero"], size = 1,
          prob = fixed_emission_param_vec["sero_prob"],
          log = T
        )
      }
      # body temperature
      if(!is.na(emission["body_temp"])){
        res = res + dnorm(
          x = emission["body_temp"],
          mean = estimated_emission_param_vec["body_temp_mean"],
          sd = exp(estimated_emission_param_vec["log_body_temp_sd"]),
          log = T)
      }
      # death indicator
      if(emission["death"]==1){
        res = res + dbinom(
          x = emission["death"], size = 1,
          prob = fixed_emission_param_vec["death_prob"],
          log = T
        )
      }
      return(res)
    }else{return(0)}
  },
  # log prior for the emission parameters
  emission_log_prior = function(estimated_emission_param_mat){
    # using MVN to impose equality on S and R
    DMVNGroup(x = estimated_emission_param_mat["body_temp_mean",],
              mean = c(37, 39, 0), mar_var = c(1, 1, .1),
              groups = list(c("S", "R"), "I", "D")) +
    DMVNGroup(x = estimated_emission_param_mat["log_body_temp_sd",],
              mean = c(0,0,0), mar_var = c(1,1,1),
              groups = list(c("S", "R"), "I", "D"))
  }
)



# Second step of model

# Modifying possible transitions
model$transition_model$to_specify$possible_transitions["S","R"]=FALSE
model$transition_model$to_specify$possible_transitions["I","S"]=FALSE
model$transition_model$to_specify$possible_transitions["R","I"]=FALSE
model$transition_model$to_specify$possible_transitions["D",c("S", "I", "R")]=FALSE
print(model$transition_model$to_specify$possible_transitions)
plotTransitionGraph(model)

## Modifying variable basis interaction table
print(model$transition_model$to_specify$BTF_per_state)
model$transition_model$to_specify$BTF_per_state$I = makeBTF(c(5, 10, 15))
model$transition_model$to_specify$BTF_per_state$R = makeBTF(c(50, 100, 150))

# Fixing parameters for the emission distribution 1e-
model$emission_model$to_specify$fixed_emission_params["pcr_prob", c("S","I", "R", "D")] =   c(1e-6,1,1e-6,1e-6)
model$emission_model$to_specify$fixed_emission_params["sero_prob", c("S","I", "R", "D")] =  c(1e-6,1,1,1e-6)
model$emission_model$to_specify$fixed_emission_params["death_prob", c("S","I", "R", "D")] = c(1e-6,1e-6,1e-6,1)


model = initializeModelStep2(model)

# Third step

model$transition_model$to_specify$explanatory_variables_effects$S[,"simple effect"]=T
model$transition_model$to_specify$explanatory_variables_effects$I["Intercept","BTF interaction"]=T
model$transition_model$to_specify$explanatory_variables_effects$I["day_temperature","no effect"]=T
model$transition_model$to_specify$explanatory_variables_effects$R["Intercept","BTF interaction"]=T
model$transition_model$to_specify$explanatory_variables_effects$R["day_temperature","no effect"]=T
model = initializeModelStep3(model)





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


