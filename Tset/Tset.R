# creating tbflist
temporal_basis_function_list = list(
  "simple_tbf" = MakeTBF(),
  "complicated_tbf" = MakeTBF(breakpoints = 2^seq(1, 8))
)
# plotting tbflist
PlotTBF(temporal_basis_function_list$simple_tbf)
PlotTBF(temporal_basis_function_list$complicated_tbf)

# initializing transition model
latent_states_names = c("S", "I", "R")
explanatory_variable_names = c("Intercept", "days_temperature")
transition_model = BeginTransitionModel(
  explanatory_variable_names = explanatory_variable_names,
  temporal_basis_function_list = temporal_basis_function_list,
  latent_states_names = latent_states_names)
# Modifying variable basis interaction table
print(transition_model$variable_basis_interactions)
transition_model$variable_basis_interactions[c("I", "R"),"days_temperature"] = NA
transition_model$variable_basis_interactions[c("I", "R"),"Intercept"] = "complicated_tbf"
print(transition_model$variable_basis_interactions)

# Looking at possible transitions
PlotTransitionGraph(transition_model)

# Modifying possible transitions
transition_model$possible_transitions["S","R"]=0
transition_model$possible_transitions["I","S"]=0
transition_model$possible_transitions["R","I"]=0
print(transition_model$possible_transitions)
PlotTransitionGraph(transition_model)


# adding emission parameters to create full model
full_model = AddEmissionModel(
  transition_model,
  fixed_emission_param_names = c("PCR+log_prob"),
  estimated_emission_param_names = c("temperature_mean", "temperature_sd", "just_walked_mean"),
  explanatory_variable_names = c("PCR", "just_walked"),
  observerd_variables_names = c("body_temp", "PCR+"))

full_model$emission$log_likelihood = function(
    obs,
    estimated_param,
    fixed_param,
    explanatory_variables){
    res = dnorm(
      x = obs[1],
      mean = estimated_param["temperature_mean"] +
        explanatory_variables["just_walked"] * estimated_param["just_walked"],
      sd = estimated_param["temperature_sd"],
      log = T)
    if(explanatory_variable["PCR"]){
      res = res +
      fixed_param["PCR+log_prob"] * obs["PCR+"] +
      log(1 - exp(fixed_param["PCR+log_prob"])) * (1 - explanatory_variables["PCR"])
    }
}




data_list = list(list("explanatory_variables_emission" = NULL , "explanatory_variables_transition" = NULL, "observations" = NULL))


# creating transition parameters with the format deduced from the transition model
transition_model_params = CreateTransitionParams(transition_model)

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
