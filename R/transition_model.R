
# transition_model = beginTransitionModel(c("Intercept", "X1", "X2"), c("S", "I", "R", "D"))

beginTransitionModel = function(
    explanatory_variable_names, latent_states_names
){
  # graph of possible transitions between latent states
  possible_transitions = matrix(TRUE, length(latent_states_names), length(latent_states_names))
  row.names(possible_transitions) = latent_states_names
  colnames(possible_transitions) = latent_states_names
  names(dimnames(possible_transitions)) = c("from", "to")
  # Empty list of BTF for each latent state
  BTF_per_state <- lapply(latent_states_names, function(x)NULL)
  names(BTF_per_state) <- latent_states_names
  # Covariates effects per state
  covariate_effect_per_state <- lapply(
    latent_states_names, function(x){
      res = matrix(FALSE, length(explanatory_variable_names), 3)
      row.names(res) = explanatory_variable_names
      colnames(res) = c("not used", "used", "used * BTF")
      res[,2] = TRUE
      res
    }
  )
  names(covariate_effect_per_state) = latent_states_names
  return(list(
    to_specify =
      list(
        "possible_transitions" = possible_transitions,
        "BTF_per_state" = BTF_per_state
      ),
    dont_touch =
      list(
        "latent_states_names" = latent_states_names,
        "n_latent_states" = length(latent_states_names),
        "explanatory_variable_names" = explanatory_variable_names)
  ))
}


# makeBTF()
# makeBTF(1)
# makeBTF(3)
# makeBTF(c(6, 3))

makeBTF = function(breakpoints = NULL, name = NULL){
  if(!is.null(breakpoints)){
    if(
      (!is.vector(breakpoints) | !all(breakpoints>1) | any(breakpoints - round(breakpoints) != 0))
    )stop(paste("Error in", name, "BTF: breakpoints must be a vector of integers greater than 1 or NULL"))
  }
  breakpoints = sort(unique(breakpoints))
  return(breakpoints)
}

#' plotBTF(makeBTF(c(3, 10, 100, 1000)))
plotBTF = function(BTF, log.x = T){
  if(!is.null(BTF)){

    BTF = c(1, BTF)
    plot(1, 1, type = "n", ylim = c(0,1), xlim = c(1, max(max(BTF)*1.1, max(BTF) + 2)), log = c("", "x")[1+log.x],
         ylab = "temporal basis function value", xlab = "time counter")
    abline(h=1, col = "gray")
    abline(h=0, col = "gray")
    abline(v=0, col = "gray")
    abline(v=max(BTF), col = 1)
    abline(v=max(BTF), col = 1)
    text(x = max(BTF)*1.05, y = .5, labels = "constant after this time counter", srt = 90)
    for(i in seq(1, length(BTF)-1)){
      points(seq(BTF[i], BTF[i+1]), seq(0,1, length.out = BTF[i+1]- BTF[i] +1), type = "b", col = i)
    }
    if(length(BTF)>2){
      for(i in seq(2, length(BTF)-1)){
        points(seq(BTF[i], BTF[i+1]), seq(1,0, length.out = BTF[i+1]- BTF[i] +1), type = "b", col = i-1)
      }
    }
  }
}


# transition_model = beginTransitionModel(c("Intercept", "X1", "X2"), c("S", "I", "R", "D"))
# transition_mat = transition_model$to_specify$possible_transitions
# latent_states_names = transition_model$dont_touch$latent_states_names
# checkTransitionMat(transition_mat, latent_states_names)
# transition_mat[1,1]=F
# checkTransitionMat(transition_mat, latent_states_names)
# transition_mat[1,1]=T
# transition_mat[1,2]=F
# checkTransitionMat(transition_mat, latent_states_names)
# transition_mat[1,2]="a"
# checkTransitionMat(transition_mat, latent_states_names)
# transition_mat[1,2]=F
# colnames(transition_mat) = NULL
# checkTransitionMat(transition_mat, latent_states_names)

checkTransitionMat <- function(transition_model){
  latent_states_names = transition_model$dont_touch$latent_states_names
  transition_mat = transition_model$to_specify$possible_transitions
  n <- length(latent_states_names)

  if (nrow(transition_mat) != n || ncol(transition_mat) != n) {
    stop(sprintf(
      "<your model>$transition_model$to_specify$possible_transitions must be a %dx%d matrix (one row and column per latent state), but found %dx%d.",
      n, n, nrow(transition_mat), ncol(transition_mat)
    ))
  }

  if (!identical(rownames(transition_mat), latent_states_names)) {
    stop(sprintf(
      "<your model>$transition_model$to_specify$possible_transitions row names do not match latent_states_names.\n  Expected: %s\n  Found:    %s",
      paste(latent_states_names,      collapse = ", "),
      paste(rownames(transition_mat), collapse = ", ")
    ))
  }

  if (!identical(colnames(transition_mat), latent_states_names)) {
    stop(sprintf(
      "<your model>$transition_model$to_specify$possible_transitions column names do not match latent_states_names.\n  Expected: %s\n  Found:    %s",
      paste(latent_states_names,      collapse = ", "),
      paste(colnames(transition_mat), collapse = ", ")
    ))
  }

  if (!is.logical(transition_mat)) stop("<your model>$transition_model$to_specify$possible_transitions must be logical")
  if (any(!diag(transition_mat)))  stop("the diagonal of <your model>$transition_model$to_specify$possible_transitions must be all TRUE")
}


# transition_model = beginTransitionModel(c("Intercept", "X1", "X2"), c("S", "I", "R", "D"))
# transition_model$to_specify$BTF_per_state$R = makeBTF(c(3,100))
# transition_model$to_specify$BTF_per_state$I = makeBTF(c(3,100))
# checkBTF(transition_model$dont_touch$latent_states_names, transition_model$to_specify$BTF_per_state)

checkBTF <- function(transition_model) {
  latent_states_names = transition_model$dont_touch$latent_states_names
  BTF_list = transition_model$to_specify$BTF_per_state
  # Check 1: names of the list match the character vector
  list_names <- names(BTF_list)

  if (is.null(list_names)) {
    stop("<your model>$transition_model$to_specify$BTF_per_state has no names.")
  }

  if (!identical(sort(list_names), sort(latent_states_names))) {
    stop(sprintf(
      "<your model>$transition_model$to_specify$BTF_per_state latent states names do not match the expected latent states names.\n  Expected: %s\n  Got:      %s",
      paste(sort(latent_states_names), collapse = ", "),
      paste(sort(list_names), collapse = ", ")
    ))
  }

  # Check 2: each non-NULL element is an increasing integer vector with all values > 1
  for (nm in list_names) {
    x <- BTF_list[[nm]]

    # NULL members are allowed — skip
    if (is.null(x)) next

    # Must be numeric / integer
    if (!is.numeric(x) && !is.integer(x)) {
      stop(sprintf("<your model>$transition_model$to_specify$BTF_per_state element '%s' is not numeric.", nm))
    }

    # Must contain only whole numbers
    if (!all(x == floor(x))) {
      stop(sprintf("<your model>$transition_model$to_specify$BTF_per_state element '%s' contains non-integer values.", nm))
    }

    # All values must be greater than 1
    if (!all(x > 1)) {
      stop(sprintf(
        "<your model>$transition_model$to_specify$BTF_per_state element '%s' contains values <= 1: %s",
        nm, paste(x[x <= 1], collapse = ", ")
      ))
    }

    # Must be strictly increasing
    if (length(x) > 1 && !all(diff(x) > 0)) {
      stop(sprintf("<your model>$transition_model$to_specify$BTF_per_state element '%s' is not strictly increasing.", nm))
    }
  }

  invisible(TRUE)
}


# transition_model = beginTransitionModel(c("Intercept", "X1", "X2"), c("S", "I", "R", "D"))
# transition_model$to_specify$BTF_per_state$I = makeBTF(c(10, 20))
# transition_model$to_specify$BTF_per_state$R = makeBTF(c(100, 200))
# transition_model$to_specify$possible_transitions["S",] = c(T,T,F,T)
# transition_model$to_specify$possible_transitions["I",] = c(F,T,T,T)
# transition_model$to_specify$possible_transitions["R",] = c(T,F,T,T)
# transition_model$to_specify$possible_transitions["D",] = c(F,F,F,T)
# continueTransitionModel(transition_model)

continueTransitionModel = function(transition_model){
  checkBTF(transition_model)
  checkTransitionMat(transition_model)
  transition_model$dont_touch = c(transition_model$dont_touch, transition_model$to_specify)
  transition_model$to_specify = NULL
  transition_model$to_specify = list(
    transition_effects =
      lapply(
        transition_model$dont_touch$latent_states_names,
        function(name){
          if(sum(transition_model$dont_touch$possible_transitions[name,])==1)return(NULL)
          if(is.null(transition_model$dont_touch$BTF_per_state[[name]])){
            out = matrix(FALSE, length(transition_model$dont_touch$explanatory_variable_names), 2)
            row.names(out) = transition_model$dont_touch$explanatory_variable_names
            colnames(out) = c("no effect", "simple effect")
            out[,2] = T
            return(out)
          }
          if(!is.null(transition_model$dont_touch$BTF_per_state[[name]])){
            out = matrix(FALSE, length(transition_model$dont_touch$explanatory_variable_names), 3)
            row.names(out) = transition_model$dont_touch$explanatory_variable_names
            colnames(out) = c("no effect", "simple effect","BTF interaction")
            out[,3] = T
            return(out)
          }
        })
  )
  transition_model
}

plotTransitionGraph = function(model){
  g = model$transition_model$to_specify$possible_transitions
  diag(g) = 0
  g = igraph::graph_from_adjacency_matrix(g)
  plot(g)
}

createExplanatoryVariablesEffects = function(transition_model){
  transition_model$dont_touch = c(transition_model$dont_touch, transition_model$to_specify)
  transition_model$to_specify = list()
  transition_model$to_specify$explanatory_variables_effects = lapply(
      transition_model$dont_touch$latent_states_names, function(latent_states_name){
        if(sum(transition_model$dont_touch$possible_transitions[latent_states_name,])==1)return(NULL)
        if(is.null(transition_model$dont_touch$BTF_per_state[[latent_states_name]])){
          res = matrix(F, length(transition_model$dont_touch$explanatory_variable_names), 2)
          row.names(res) = transition_model$dont_touch$explanatory_variable_names
          colnames(res) =  c("no effect", "simple effect")
          return(res)
        }
        if(!is.null(transition_model$dont_touch$BTF_per_state[[latent_states_name]])){
          res = matrix(F, length(transition_model$dont_touch$explanatory_variable_names), 3)
          row.names(res) = transition_model$dont_touch$explanatory_variable_names
          colnames(res) =  c("no effect", "simple effect", "BTF interaction")
          return(res)
        }
      }
    )
  names(transition_model$to_specify$explanatory_variables_effects) = transition_model$dont_touch$latent_states_names
  return(transition_model)
}


checkExplanatoryVariablesEffects = function(transition_model){
  lapply(
    transition_model$dont_touch$latent_states_names, function(latent_states_name){
      if(sum(transition_model$dont_touch$possible_transitions[latent_states_name,])==1){
        if(!is.null(transition_model$to_specify$explanatory_variables_effects[[latent_states_name]]))stop(paste("<your model>$transition_model$to_specify$explanatory_variables_effects$", latent_states_name, "must be null"))
        return(invisible())
      }
      if(is.null(transition_model$dont_touch$BTF_per_state[[latent_states_name]])){
        if(!is.logical(transition_model$to_specify$explanatory_variables_effects[[latent_states_name]]))stop(paste("<your model>$transition_model$to_specify$explanatory_variables_effects$", latent_states_name, "must be logical"))
        if(!any(transition_model$to_specify$explanatory_variables_effects[[latent_states_name]][,2]))stop(paste("The 'simple effect' column of <your model>$transition_model$to_specify$explanatory_variables_effects$", latent_states_name, "must have at least one TRUE slot"))
        if(any(apply(transition_model$to_specify$explanatory_variables_effects[[latent_states_name]], 1, sum)!=1))stop(paste("The matrix at <your model>$transition_model$to_specify$explanatory_variables_effects$", latent_states_name, "must have exactly one TRUE per row"))

        return(invisible())
      }
      if(!is.null(transition_model$dont_touch$BTF_per_state[[latent_states_name]])){
        if(!is.logical(transition_model$to_specify$explanatory_variables_effects[[latent_states_name]]))stop(paste("<your model>$transition_model$to_specify$explanatory_variables_effects$", latent_states_name, "must be logical"))
        if(!any(transition_model$to_specify$explanatory_variables_effects[[latent_states_name]][,3]))stop(paste("The 'BTF interaction' column of <your model>$transition_model$to_specify$explanatory_variables_effects$", latent_states_name, "must have at least one TRUE slot"))
        if(any(apply(transition_model$to_specify$explanatory_variables_effects[[latent_states_name]], 1, sum)!=1))stop(paste("The matrix at <your model>$transition_model$to_specify$explanatory_variables_effects$", latent_states_name, "must have exactly one TRUE per row"))
        return(invisible())
      }
    }
  )
  return(invisible())
}

