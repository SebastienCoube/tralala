plotLatentState <- function(model, ind_idx, add_time_spent = T){
  if(add_time_spent)par(mfrow = c(2,1))
  par(mar =c(2,1,3,1))
  plot(match(model$data_list[[ind_idx]]$latent_states, model$transition_model$latent_states),
       ylim = c(1, length(model$transition_model$latent_states)+.3), type = "b", ylab = "",
       xlab = "time index", main = "Latent state")
  for(i in seq(length(model$transition_model$latent_states))){
    text(model$transition_model$latent_states[i], x = 0, y = i+.2)
  }
  if(add_time_spent){
    par(mar =c(2,4,3,1))
    plot(model$data_list[[ind_idx]]$time_spent,
         type = "l", ylab = "", main = "Time spent in latent state",
         xlab = "time index", log = "y")
  }
}
