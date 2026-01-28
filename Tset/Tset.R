# creating tbflist
temporal_basis_function_list = list(
  "simple_tbf" = makeTbf(),
  "complicated_tbf" = makeTbf(breakpoints = 2^seq(1, 8))
)
# plotting tbflist
plotTbf(temporal_basis_function_list$simple_tbf)
plotTbf(temporal_basis_function_list$complicated_tbf)

# initializing transition model
latent_states_names = c("S", "I", "R")

temperature_series = 20 + 4 * GpGp::fast_Gp_sim(covparms = c(1,10,1.5, 0), locs = cbind(seq(2000), 1), m = 5) + 3 * rnorm(2000) + 12 * sin(seq(2000)*2*pi/365)
plot(temperature_series)
data_list = list(
  individual_1 = list(
    explanatory_variables_transition = matrix(cbind(1, temperature_series), ncol= 2, dimnames = list(NULL, c("Intercept", "day_temperature")) ),
    emissions = lapply(seq(2000), function(x)NA)
  ),
  individual_2 = list(
    explanatory_variables_transition = matrix(cbind(1, temperature_series), ncol= 2, dimnames = list(NULL, c("Intercept", "day_temperature")) ),
    emissions = lapply(seq(2000), function(x)NA)
  )
)

plot(data_list$individual_1$explanatory_variables_transition[,2])

model = beginModel(
  latent_states_names = c("S", "I", "R"),
  data_list = data_list,
  temporal_basis_function_list = temporal_basis_function_list,
  fixed_emission_param_names = c("logPCR+prob"),
  estimated_emission_param_names = c("body_temp_mean", "log_body_temp_sd"),
  emission_log_likelihood = function(estimated_emission_param_vec, fixed_emission_param_vec, emission, explanatory_variable_vec){
    if(!is.na(emission)){
      res=  0
      if(!is.na(emission["PCR"])){
        res = res +
          fixed_emission_param_vec["logPCR+prob"]*emission$PCR +
          log(1 - exp(fixed_emission_param_vec["logPCR+prob"]))*(1-emission$PCR)
      }
      res = res + dnorm(x = emission["body_temp"], mean = estimated_emission_param_vec["body_temp_mean"], sd = exp(estimated_emission_param_vec["log_body_temp_sd"]), log = T)
    }else{return(0)}
  },
  emission_log_prior = function(estimated_emission_param_mat){
    # using MVN to impose equality on S and R
    cov_mean = matrix(c(.5,0,.49, 0, .5, 0, .49, 0, .5), 3)
    mean_mean = c(37,39,37)
    cov_logsd = matrix(c(2,0,1.99, 0, 2, 0, 1.99, 0, 2), 3)
    return(
      -.5 * (
        sum(((estimated_emission_param_mat["body_temp_mean"]-mean_mean) %*% solve(cov_mean)) * (estimated_emission_param_mat["body_temp_mean"]-mean_mean)) +
          sum(estimated_emission_param_mat["log_body_temp_sd"]%*% solve(cov_mean) * (estimated_emission_param_mat["log_body_temp_sd"]))
      )
    )
  }
)


# Modifying variable basis interaction table
print(model$transition_model$to_specify$variable_basis_interactions)
model$transition_model$to_specify$variable_basis_interactions[c("I", "R"),"day_temperature"] = NA
model$transition_model$to_specify$variable_basis_interactions[c("I", "R"),"Intercept"] = "complicated_tbf"
model$transition_model$to_specify$variable_basis_interactions["S",] = "simple_tbf"
print(model$transition_model$to_specify$variable_basis_interactions)

# Looking at possible transitions
plotTransitionGraph(model)

# Modifying possible transitions
model$transition_model$to_specify$possible_transitions["S","R"]=0
model$transition_model$to_specify$possible_transitions["I","S"]=0
model$transition_model$to_specify$possible_transitions["R","I"]=0
print(model$transition_model$to_specify$possible_transitions)
plotTransitionGraph(model)




# creating transition parameters with the format deduced from the transition model
params = createParams(model)

# adding value for the transition parameters
transition_model_params$S$Intercept =-4
transition_model_params$S$days_temperature = 1
transition_model_params$I$Intercept[1] = -4
transition_model_params$I$Intercept[2] = 1
transition_model_params$I$Intercept[3] = 1
transition_model_params$R$Intercept[1] = -8
transition_model_params$R$Intercept[8] = .1
transition_model_params$R$Intercept[9] = .1

explanatory_variables = matrix(0, 2000, 2)
colnames(explanatory_variables) = c("Intercept", "days_temperature")
explanatory_variables[,1]=1
explanatory_variables[,2]= sin(seq(nrow(explanatory_variables))*(2*pi)/365) * 15 + 20 + 2*rnorm(nrow(explanatory_variables)) + GpGp::fast_Gp_sim(c(20,10,1.5,0), covfun_name = "matern_isotropic", locs = cbind(1, seq(nrow(explanatory_variables))), m = 10)
plot(explanatory_variables[,2])


starting_states = rep("S", 100)
current_latent_state = "I"
current_counter_var = 1



observations = list(
  obs1 = list("emissions" = ...., "explanatory_variables" = ....),
  obs2 = list("emissions" = ...., "explanatory_variables" = ....),
  obs3 = list("emissions" = ...., "explanatory_variables" = ....),
  obs4 = list("emissions" = ...., "explanatory_variables" = ....),
  obs5 = list("emissions" = ...., "explanatory_variables" = ....)
)
fit_parameters = FitParameters(full_model, observations)
