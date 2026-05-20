
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
        "BTF_per_state" = BTF_per_state,
        "covariate_effect_per_state" = covariate_effect_per_state
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

checkTransitionMat <- function(transition_mat, latent_states_names){

  n <- length(latent_states_names)

  if (nrow(transition_mat) != n || ncol(transition_mat) != n) {
    stop(sprintf(
      "transition_mat must be a %dx%d matrix (one row and column per latent state), but found %dx%d.",
      n, n, nrow(transition_mat), ncol(transition_mat)
    ))
  }

  if (!identical(rownames(transition_mat), latent_states_names)) {
    stop(sprintf(
      "transition_mat row names do not match latent_states_names.\n  Expected: %s\n  Found:    %s",
      paste(latent_states_names,      collapse = ", "),
      paste(rownames(transition_mat), collapse = ", ")
    ))
  }

  if (!identical(colnames(transition_mat), latent_states_names)) {
    stop(sprintf(
      "transition_mat column names do not match latent_states_names.\n  Expected: %s\n  Found:    %s",
      paste(latent_states_names,      collapse = ", "),
      paste(colnames(transition_mat), collapse = ", ")
    ))
  }

  if (!is.logical(transition_mat)) stop("transition_model$to_specify$possible_transitions must be logical")
  if (any(!diag(transition_mat)))  stop("the diagonal of transition_model$to_specify$possible_transitions must be all TRUE")
}

# transition_model = beginTransitionModel(c("Intercept", "X1", "X2"), c("S", "I", "R", "D"))
# checkCovariateEffectPerState(
#   latent_states_names = transition_model$dont_touch$latent_states_names,
#   explanatory_variable_names = transition_model$dont_touch$explanatory_variable_names,
#   effectPerStateList = transition_model$to_specify$covariate_effect_per_state)
# names(transition_model$to_specify$covariate_effect_per_state)[[1]]="q"
# checkCovariateEffectPerState(
#   latent_states_names = transition_model$dont_touch$latent_states_names,
#   explanatory_variable_names = transition_model$dont_touch$explanatory_variable_names,
#   effectPerStateList = transition_model$to_specify$covariate_effect_per_state)
# transition_model = beginTransitionModel(c("Intercept", "X1", "X2"), c("S", "I", "R", "D"))
# transition_model$to_specify$covariate_effect_per_state[[1]][,1]=T
# checkCovariateEffectPerState(
#   latent_states_names = transition_model$dont_touch$latent_states_names,
#   explanatory_variable_names = transition_model$dont_touch$explanatory_variable_names,
#   effectPerStateList = transition_model$to_specify$covariate_effect_per_state)
# transition_model$to_specify$covariate_effect_per_state[[1]][,1]=T
# transition_model$to_specify$covariate_effect_per_state[[1]][,2]=F
# checkCovariateEffectPerState(
#   latent_states_names = transition_model$dont_touch$latent_states_names,
#   explanatory_variable_names = transition_model$dont_touch$explanatory_variable_names,
#   effectPerStateList = transition_model$to_specify$covariate_effect_per_state)

checkCovariateEffectPerState = function(effectPerStateList, explanatory_variable_names, latent_states_names) {
  if (!identical(names(effectPerStateList), latent_states_names)) {
    stop(sprintf(
      "Error while checking transition_model$to_specify$covariate_effect_per_state: list names do not match the expected ones.\n  Expected: %s\n  Found:    %s",
      paste(latent_states_names, collapse = ", "),
      paste(names(effectPerStateList),     collapse = ", ")
    ))
  }

  for (list_name in names(effectPerStateList)) {
    mat <- effectPerStateList[[list_name]]

    if (ncol(mat) != 3) {
      stop(sprintf(
        "Error while checking transition_model$to_specify$covariate_effect_per_state: element '%s' does not have exactly 3 columns (found %d).",
        list_name, ncol(mat)
      ))
    }

    if (!identical(rownames(mat), explanatory_variable_names)) {
      stop(sprintf(
        "Error while checking transition_model$to_specify$covariate_effect_per_state: element '%s': row names do not match the expected ones.\n  Expected: %s\n  Found:    %s",
        list_name,
        paste(explanatory_variable_names, collapse = ", "),
        paste(rownames(mat),     collapse = ", ")
      ))
    }

    for (row_name in rownames(mat)) {
      row_vals <- mat[row_name, ]
      n_true   <- sum(row_vals == TRUE)
      n_false  <- sum(row_vals == FALSE)

      if (n_true != 1L || n_false != 2L) {
        stop(sprintf(
          "Error while checking transition_model$to_specify$covariate_effect_per_state: element '%s', row '%s': expected exactly one TRUE and two FALSE, but found %d TRUE and %d FALSE.",
          list_name, row_name, n_true, n_false
        ))
      }
    }
  }
  invisible(TRUE)
}


# transition_model = beginTransitionModel(c("Intercept", "X1", "X2"), c("S", "I", "R", "D"))
# transition_model$to_specify$BTF_per_state$R = makeBTF(c(3,100))
# transition_model$to_specify$BTF_per_state$I = makeBTF(c(3,100))
# checkBTF(transition_model$dont_touch$latent_states_names, transition_model$to_specify$BTF_per_state)

checkBTF <- function(latent_states_names, BTF_list) {
  # Check 1: names of the list match the character vector
  list_names <- names(BTF_list)

  if (is.null(list_names)) {
    stop("model$transition_model$to_specify$BTF_per_state has no names.")
  }

  if (!identical(sort(list_names), sort(latent_states_names))) {
    stop(sprintf(
      "model$transition_model$to_specify$BTF_per_state latent states names do not match the expected latent states names.\n  Expected: %s\n  Got:      %s",
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
      stop(sprintf("model$transition_model$to_specify$BTF_per_state element '%s' is not numeric.", nm))
    }

    # Must contain only whole numbers
    if (!all(x == floor(x))) {
      stop(sprintf("model$transition_model$to_specify$BTF_per_state element '%s' contains non-integer values.", nm))
    }

    # All values must be greater than 1
    if (!all(x > 1)) {
      stop(sprintf(
        "model$transition_model$to_specify$BTF_per_state element '%s' contains values <= 1: %s",
        nm, paste(x[x <= 1], collapse = ", ")
      ))
    }

    # Must be strictly increasing
    if (length(x) > 1 && !all(diff(x) > 0)) {
      stop(sprintf("model$transition_model$to_specify$BTF_per_state element '%s' is not strictly increasing.", nm))
    }
  }

  invisible(TRUE)
}

# transition_model = beginTransitionModel(c("Intercept", "X1", "X2"), c("S", "I", "R", "D"))
# checkTransitionModelCoherence(
#   BTF_per_state = transition_model$to_specify$BTF_per_state,
#   covariate_effect_per_state = transition_model$to_specify$covariate_effect_per_state,
#   transition_mat = transition_model$to_specify$possible_transitions,
#   latent_states_names =  transition_model$dont_touch$latent_states_names
# )
# transition_model$to_specify$BTF_per_state$I = makeBTF(c(10, 100))
# checkTransitionModelCoherence(
#   BTF_per_state = transition_model$to_specify$BTF_per_state,
#   covariate_effect_per_state = transition_model$to_specify$covariate_effect_per_state,
#   transition_mat = transition_model$to_specify$possible_transitions,
#   latent_states_names =  transition_model$dont_touch$latent_states_names
# )
# transition_model$to_specify$covariate_effect_per_state$I[1,] = c(F,F,T)
# checkTransitionModelCoherence(
#   BTF_per_state = transition_model$to_specify$BTF_per_state,
#   covariate_effect_per_state = transition_model$to_specify$covariate_effect_per_state,
#   transition_mat = transition_model$to_specify$possible_transitions,
#   latent_states_names =  transition_model$dont_touch$latent_states_names
# )
# transition_model$to_specify$covariate_effect_per_state$S[,2] = F
# transition_model$to_specify$covariate_effect_per_state$S[,1] = T
# checkTransitionModelCoherence(
#   BTF_per_state = transition_model$to_specify$BTF_per_state,
#   covariate_effect_per_state = transition_model$to_specify$covariate_effect_per_state,
#   transition_mat = transition_model$to_specify$possible_transitions,
#   latent_states_names =  transition_model$dont_touch$latent_states_names
# )
# transition_model$to_specify$covariate_effect_per_state$S[1,] = c(F,T,F)
# checkTransitionModelCoherence(
#   BTF_per_state = transition_model$to_specify$BTF_per_state,
#   covariate_effect_per_state = transition_model$to_specify$covariate_effect_per_state,
#   transition_mat = transition_model$to_specify$possible_transitions,
#   latent_states_names =  transition_model$dont_touch$latent_states_names
# )
# transition_model$to_specify$possible_transitions["D",] = c(F,F,F,T)
# checkTransitionModelCoherence(
#   BTF_per_state = transition_model$to_specify$BTF_per_state,
#   covariate_effect_per_state = transition_model$to_specify$covariate_effect_per_state,
#   transition_mat = transition_model$to_specify$possible_transitions,
#   latent_states_names =  transition_model$dont_touch$latent_states_names
# )


checkTransitionModelCoherence <- function(latent_states_names, transition_mat, covariate_effect_per_state, BTF_per_state) {
  for (state in latent_states_names) {
    row     <- transition_mat[state, ]
    cov_mat <- covariate_effect_per_state[[state]]
    btf_vec <- BTF_per_state[[state]]

    # Case 1: only the diagonal is TRUE in the transition row
    if (sum(row) == 1 && row[state]) {

      # All columns except the first must be FALSE
      if (ncol(cov_mat) > 1 && any(cov_mat[, -1, drop = FALSE])) {
        stop(sprintf(
          "State '%s': only the diagonal of transition_mat is TRUE, so only the first column of covariate_effect_per_state can be TRUE.",
          state
        ))
      }

      # BTF element must be NULL
      if (!is.null(btf_vec)) {
        stop(sprintf(
          "State '%s': only the diagonal of transition_mat is TRUE, so the corresponding element of BTF_per_state must be NULL.",
          state
        ))
      }

      # Cases 2 & 3: more than the diagonal is TRUE in the transition row
    } else if (sum(row) > 1) {

      # Case 2: BTF element is NULL
      if (is.null(btf_vec)) {

        # Third column and beyond must be FALSE
        if (ncol(cov_mat) > 2 && any(cov_mat[, -(1:2), drop = FALSE])) {
          stop(sprintf(
            "State '%s': transition_mat has multiple TRUE entries and BTF_per_state is NULL, so the third column and beyond of covariate_effect_per_state must be FALSE.",
            state
          ))
        }

        # Second column must have at least one TRUE
        if (ncol(cov_mat) < 2 || !any(cov_mat[, 2])) {
          stop(sprintf(
            "State '%s': transition_mat has multiple TRUE entries and BTF_per_state is NULL, so the second column of covariate_effect_per_state must have at least one TRUE.",
            state
          ))
        }

        # Case 3: BTF element is not NULL
      } else {

        # All columns of covariate_effect_per_state are allowed — no column restriction

        # Third column must have at least one TRUE
        if (ncol(cov_mat) < 3 || !any(cov_mat[, 3])) {
          stop(sprintf(
            "State '%s': transition_mat has multiple TRUE entries and BTF_per_state is not NULL, so the third column of covariate_effect_per_state must have at least one TRUE.",
            state
          ))
        }
      }
    }
  }

  invisible(TRUE)
}

checkTransitionModel = function(transition_model){
  checkBTF(transition_model$dont_touch$latent_states_names, transition_model$to_specify$BTF_per_state)
  checkTransitionMat(transition_mat, latent_states_names)
  checkCovariateEffectPerState(
    latent_states_names = transition_model$dont_touch$latent_states_names,
    explanatory_variable_names = transition_model$dont_touch$explanatory_variable_names,
    effectPerStateList = transition_model$to_specify$covariate_effect_per_state)
  checkTransitionModelCoherence(
    BTF_per_state = transition_model$to_specify$BTF_per_state,
    covariate_effect_per_state = transition_model$to_specify$covariate_effect_per_state,
    transition_mat = transition_model$to_specify$possible_transitions,
    latent_states_names =  transition_model$dont_touch$latent_states_names
  )
}
