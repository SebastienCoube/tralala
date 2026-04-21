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
  fixed_emission_param_names = c("PCR_prob", "isded_prob"),
  # emission parameters whose value is estimated by the model
  estimated_emission_param_names = c("body_temp_mean", "log_body_temp_sd"),
  # log-likelihood of an emission
  emission_log_likelihood = function(
    estimated_emission_param_vec,
    fixed_emission_param_vec,
    emission,
    explanatory_variable_vec){
    if(!is.na(emission)){
      res = 0
      if(!is.null(emission["PCR"])){
        res = res + dbinom(
          x = emission["PCR"], size = 1,
          prob = fixed_emission_param_vec["PCR_prob"],
          log = T
        )
      }
      res = res + dbinom(
        x = emission["isded"], size = 1,
        prob = fixed_emission_param_vec["isded_prob"],
        log = T
      )
      res = res + dnorm(x = emission["body_temp"], mean = estimated_emission_param_vec["body_temp_mean"], sd = exp(estimated_emission_param_vec["log_body_temp_sd"]), log = T)
      return(res)
    }else{return(0)}
  },
  # log prior for the emission parameters
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


groups = c("S"=1, "I"=2, "R"=1, "D"=3)
logMVDnorm = function(x, mean_per_group, mar_var_per_group, groups){
  if(!identical(names(groups), names(x)))stop("the names in the vector group must be the same as the name in vector x")
  mean_ =
}

softPlus = function(x) log(1 + exp(x))
plot(seq(-20, 20), softPlus(seq(-20, 20)))
abline(a=0, b=1)
abline(h=0)

softPlusSquare = function(x) log(1 + exp(x))^2
plot(seq(-20, 20), softPlusSquare(seq(-20, 20)))
abline(a=0, b=1)
abline(h=0)


# Looking at possible transitions
plotTransitionGraph(model)
# Modifying possible transitions
model$transition_model$to_specify$possible_transitions["S","R"]=0
model$transition_model$to_specify$possible_transitions["I","S"]=0
model$transition_model$to_specify$possible_transitions["R","I"]=0
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
model$emission_model$to_specify$fixed_emission_params["logPCR+prob", "S"] = -20
model$emission_model$to_specify$fixed_emission_params["logPCR+prob", "I"] = 0
model$emission_model$to_specify$fixed_emission_params["logPCR+prob", "R"] = -20


########################################

transition_model = model$transition_model
emission_model = model$emission_model
data_seq = data_list[[1]]



# creating transition parameters with the format deduced from the transition model
params = createParams(model)

# adding value for the transition parameters
params$transition_params$S$Intercept[]      =0
params$transition_params$S$day_temperature[] = -1
params$transition_params$I$Intercept[1] = -4
params$transition_params$I$Intercept[2] = -3
params$transition_params$I$Intercept[3] = -2
params$transition_params$I$Intercept[4] = -1
params$transition_params$R$Intercept[1] = -8
params$transition_params$R$Intercept[2] = -8
params$transition_params$R$Intercept[3] = -7
params$transition_params$R$Intercept[4] = -6
params$transition_params$R$Intercept[5] = -5
params$transition_params$R$Intercept[6] = -4
params$transition_params$R$Intercept[7] = -4
params$transition_params$R$Intercept[8] = -4



