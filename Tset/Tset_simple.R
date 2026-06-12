remove(list = ls())# I dont care if you judge me
source("R/initialize_model.R")
source("R/check_data.R")
source("R/transition_model.R")
source("R/emission_model.R")


# initializing transition model

# list of data

hidden_states = c(rep("S", 100), rep("I", 10), rep("R", 100))
hidden_states = rep(hidden_states, 8)
hidden_states = c(hidden_states, rep("D"), 800)


data_list = list(
  individual_1 = list(
    explanatory_variables_transition = matrix(1, length(hidden_states)-1, 1, dimnames = list(NULL, "Intercept")),
    emissions = as.list(hidden_states)
  )
)


# First step of model
model = initializeModelStep1(
  # names of the latent states
  latent_states_names = c("S", "I", "R", "D"),
  # observations
  data_list = data_list,
  # emission parameters whose value is fixed by the user
  fixed_emission_param_names = c("S_prob", "I_prob", "R_prob", "D_prob"),
  # log-likelihood of an emission
  emission_log_likelihood = function(
    fixed_emission_param_vec,
    emission){
      fixed_emission_param_vec[match(emission, c("S", "I", "R", "D"))]
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

# Fixing parameters for the emission distribution
model$emission_model$to_specify$fixed_emission_params[,"S"] =  c(.94, .02, .02, .02)
model$emission_model$to_specify$fixed_emission_params[,"I"] =  c(.02, .94, .02, .02)
model$emission_model$to_specify$fixed_emission_params[,"R"] =  c(.02, .02, .94, .02)
model$emission_model$to_specify$fixed_emission_params[,"D"] =  c(.02, .02, .02, .94)


model = initializeModelStep2(model)

# Third step

model$transition_model$to_specify$explanatory_variables_effects$S[,"simple effect"]=T
model$transition_model$to_specify$explanatory_variables_effects$I[,"BTF interaction"]=T
model$transition_model$to_specify$explanatory_variables_effects$R[,"BTF interaction"]=T
model = initializeModelStep3(model)

params = createParams(model)
