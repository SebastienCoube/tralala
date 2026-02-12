source("R/initialize_model.R")

# creating list of temporal basis functions
temporal_basis_function_list = list(
  "simple_tbf" = makeTbf(),
  "complicated_tbf" = makeTbf(breakpoints = 2^seq(1, 8))
)
# plotting tbflist
plotTbf(temporal_basis_function_list$simple_tbf, log.x = F)
plotTbf(temporal_basis_function_list$complicated_tbf, log.x = F)
plotTbf(temporal_basis_function_list$complicated_tbf, log.x = T)

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

# initializing model
model = beginModel(
  # names of the latent states
  latent_states_names = c("S", "I", "R", "D"),
  # observations
  data_list = data_list,
  # temporal basis functions
  temporal_basis_function_list = temporal_basis_function_list,
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
    if(!is.na(emission)){
      res=  0
      # PCR test
      if(!is.na(emission["PCR"])){
        res = res + dbinom(
          x = emission["PCR"], size = 1,
          prob = fixed_emission_param_names["pcr_prob"],
          log = T
        )
      }
      # serologic test test
      if(!is.na(emission["sero"])){
        res = res + dbinom(
          x = emission["sero"], size = 1,
          prob = fixed_emission_param_names["sero_prob"],
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
          prob = fixed_emission_param_names["death_prob"],
          log = T
        )
      }
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






# Looking at possible transitions
plotTransitionGraph(model)
# Modifying possible transitions
model$transition_model$to_specify$possible_transitions["S","R"]=0
model$transition_model$to_specify$possible_transitions["I","S"]=0
model$transition_model$to_specify$possible_transitions["R","I"]=0
model$transition_model$to_specify$possible_transitions["D",c("S", "I", "R")]=0
print(model$transition_model$to_specify$possible_transitions)
# Looking at possible transitions
plotTransitionGraph(model)

# Modifying variable basis interaction table
print(model$transition_model$to_specify$variable_basis_interactions)
model$transition_model$to_specify$variable_basis_interactions[c("I", "R"),"day_temperature"] = NA
model$transition_model$to_specify$variable_basis_interactions[c("I", "R"),"Intercept"] = "complicated_tbf"
model$transition_model$to_specify$variable_basis_interactions["S",] = "simple_tbf"
print(model$transition_model$to_specify$variable_basis_interactions)

# Fixing parameters for the emission distribution
model$emission_model$to_specify$fixed_emission_params["pcr_prob", "S"] = 0
model$emission_model$to_specify$fixed_emission_params["pcr_prob", "I"] = 1
model$emission_model$to_specify$fixed_emission_params["pcr_prob", "R"] = 0
model$emission_model$to_specify$fixed_emission_params["pcr_prob", "D"] = 0

model$emission_model$to_specify$fixed_emission_params["sero_prob", "S"] = 0
model$emission_model$to_specify$fixed_emission_params["sero_prob", "I"] = 0
model$emission_model$to_specify$fixed_emission_params["sero_prob", "R"] = 1
model$emission_model$to_specify$fixed_emission_params["sero_prob", "D"] = 0

model$emission_model$to_specify$fixed_emission_params["death_prob", "S"] = 0
model$emission_model$to_specify$fixed_emission_params["death_prob", "I"] = 0
model$emission_model$to_specify$fixed_emission_params["death_prob", "R"] = 0
model$emission_model$to_specify$fixed_emission_params["death_prob", "D"] = 1


########################################

transition_model = model$transition_model
emission_model = model$emission_model
data_seq = data_list[[1]]



# creating transition parameters with the format deduced from the transition model
params = createParams(model)

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
params$transition_params$I$Intercept[5,"D"] = -2
params$transition_params$I$Intercept[6,"D"] = 1
params$transition_params$I$Intercept[7,"D"] = 1
params$transition_params$I$Intercept[8,"D"] = 1
params$transition_params$I$Intercept[9,"D"] = 1

params$transition_params$R$Intercept[1,"S"] = -8
params$transition_params$R$Intercept[2,"S"] = -8
params$transition_params$R$Intercept[3,"S"] = -7
params$transition_params$R$Intercept[4,"S"] = -6
params$transition_params$R$Intercept[5,"S"] = -5
params$transition_params$R$Intercept[6,"S"] = -4
params$transition_params$R$Intercept[7,"S"] = -4
params$transition_params$R$Intercept[8,"S"] = -4
params$transition_params$R$Intercept[,"D"] = -8



transition_model = model$transition_model
emission_model = model$emission_model
data_seq = model$data_list$individual_1


source("R/forward.R")

preprocessTbf(transition_model, params)

latent_states_sample = sampleLatentState(
  n_samples = 10, model = model, initial_state = rep("S", 10), initial_time_counter = rep(1, 10),
  params = params, data_seq = model$data_list[[1]])
par (mfrow = c(2, 1))

plotLatentState(latent_states_sample$latent_state[,2], model = model)



