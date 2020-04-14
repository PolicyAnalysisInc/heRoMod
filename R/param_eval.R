
#' Evaluate Markov model parameters
#' 
#' Evaluate parameters specified through 
#' `define_parameters`, for a given number of cycles.
#' 
#' @param x an `uneval_parameters` object.
#' @param cycles integer. Number of cycles to simulate.
#'   
#' @return An object of class `eval_parameters` 
#'   (actually a data.frame with one column per parameter 
#'   and one row per cycle).
#'   
#' @example inst/examples/example_eval_parameters.R
#'   
#' @keywords internal
eval_parameters <- function(x, cycles = 1,
                            strategy_name = '', max_state_time = cycles,
                            disc_method = 'start') {
  # update calls to dispatch_strategy()
  x <- dispatch_strategy_hack(x)
  x <- by_group_hack(x)
  x <- discount_hack(x, method = disc_method)
  
  expanding <- max_state_time > 1
  
  # Long form tibble w/ state_time and model_time
  start_tibble <- tibble::tibble(
    model_time = rep(seq_len(cycles), max_state_time),
    markov_cycle = rep(seq_len(cycles), max_state_time),
    state_time = rep(seq_len(max_state_time), each=cycles),
    strategy = strategy_name
  )
  
  # other datastructure?
  res <- safe_eval(start_tibble, .dots = x)
  
  structure(
    res,
    class = c("eval_parameters", class(res))
  )
}


eval_obj_parameters <- function(x, params) {
  
  x <- dispatch_strategy_hack(x)
  x <- by_group_hack(x)
  
  if(length(x) > 0) {
    env <- x[[1]]$env
    purrr::imap(
      x,
      function(obj, name) {
        # Try to evaluate parameter
        res <- try(lazyeval::lazy_eval(obj, data = params), silent = T)
        if(inherits(res, "try-error")) {
          
          # Check if binding not found
          if (startsWith(res, "Error in eval(x$expr, data, x$env) : object ")) {
            res <- paste0(
              sub(
                "Error in eval(x$expr, data, x$env) : object ",
                "reference to undefined variable ",
                substr(res, 0, stop = nchar(res) - 11),
                fixed = T
              ),
              "."
            )
          }
          
          # If an error occurs, relay error message with description
          # of which parameter caused it
          stop(sprintf(
            "Error in %s '%s', %s", "survival distribution", name, res),
            call. = FALSE)
        } else {
          # Assign results to environment
          assign(name, res, env)
        }
      }
    )
  }
  
}

eval_init <- function(x, parameters, expand) {
  
  # Assinging NULLS to avoid CMD Check issues
  .state <- .limit <- model_time <- state_time <- .value <- NULL
  
  x <- dispatch_strategy_hack(x)
  x <- by_group_hack(x)
  
  to_keep <- names(x)
  
  expanding <- any(expand$.expand)
  
  # Replace complement with negative pi
  parameters$C <- -pi
  
  if(expanding) {
    init_df <- parameters %>%
      filter(model_time == 1) %>%
      safe_eval(.dots = x, .vartype = "init") %>%
      .[c("state_time", to_keep)] %>%
      reshape2::melt(
        id.vars = c("state_time"),
        variable.name = ".state",
        value.name = ".value"
      ) %>%
      mutate(.state = as.character(.state)) %>%
      left_join(expand, by = c(".state" =  ".state", "state_time" = "state_time")) %>%
      filter(state_time <= .limit) %>%
      mutate(
        .value = ifelse(state_time > 1, 0, .value)
      )
  } else {
    init_df <- parameters %>%
      filter(model_time == 1) %>%
      safe_eval(.dots = x, .vartype = "init") %>%
      .[c("state_time", to_keep)] %>%
      reshape2::melt(
        id.vars = c("state_time"),
        variable.name = ".state",
        value.name = ".value"
      ) %>%
      mutate(
        .full_state = as.character(.state),
        .state = as.character(.state)
      )
  }
  
  init_vector <- init_df$.value
  names(init_vector) <- init_df$.full_state
  
  # Detect any missing values
  if(any(is.na(init_vector))) {
    error_states <- paste0("'", init_df$.full_state[is.na(init_vector)], "'")
    error_states_string <- paste0(error_states, collapse = ",")
    stop(
      paste0(
        "Error in initial probabilities, missing value detected for states: ",
        error_states_string
      ),
      call. = F
    )
  }
  
  # Calculate complementary probability if necessary
  uses_complement <- init_vector == -pi
  if(sum(uses_complement) > 1) {
    stop(
      "Error in initial probabilities, complement keyword 'C' can only be used for at most one state.",
      call. = F
    )
  } else if(sum(uses_complement == 1)) {
    init_vector[uses_complement] <- 1 - sum(init_vector[!uses_complement])
  }
  # Check that probabilities are non-negative
  if(any(init_vector < 0)) {
    error_states <- paste0("'", init_df$.full_state[init_vector < 0], "'")
    error_states_string <- paste0(error_states, collapse = ",")
    stop(
      paste0(
        "Error in initial probabilities, probabilites are negative for states: ",
        error_states_string
      ),
      call. = F
    )
  }
  
  # Check that probabilities are within [0-1]
  # if(any(init_vector < 0) || any(init_vector > 1)) {
  #   error_states <- paste0("'", init_df$.full_state[init_vector < 0 | init_vector > 1], "'")
  #   error_states_string <- paste0(error_states, collapse = ",")
  #   stop(
  #     paste0(
  #       "Error in initial probabilities, probabilites are outside range [0-1] for states: ",
  #       error_states_string
  #     ),
  #     call. = F
  #   )
  # }
  
  # Check that probabiltiies sum to 1
  # if(sum(init_vector) != 1) {
  #   stop("Error in initial probabiltiies, values do not sum to 1.", call. = F)
  # }
  
  init_vector
  
}

eval_starting_values <- function(x, parameters) {
  
  # Assinging NULLS to avoid CMD Check issues
  state_time <- NULL
  
  
  to_keep <- names(x)
  
  start_df <- parameters %>%
    filter(state_time == 1) %>%
    mutate_(.dots = x) %>%
    .[to_keep]
  
  start_df[nrow(start_df), ] <- 0
  
  start_df
  
}

eval_inflow <- function(x, parameters, expand) {
  
  # Assinging NULLS to avoid CMD Check issues
  .state <- .limit <- state_time <- .value <- NULL
  
  expanding <- any(expand$.expand)
  
  to_keep <- names(x)
  if(expanding) {
    inflow_df <- parameters %>%
      mutate(!!!lazy_eval(x, data = .)) %>%
      .[c("model_time", "state_time", to_keep)] %>%
      reshape2::melt(
        id.vars = c("model_time", "state_time"),
        variable.name = ".state",
        value.name = ".value"
      ) %>%
      mutate(.state = as.character(.state)) %>%
      left_join(expand, by = c(".state" =  ".state", "state_time" = "state_time")) %>%
      filter(state_time <= .limit) %>%
      mutate(.value = ifelse(state_time > 1, 0, .value)) %>%
      ungroup()
  } else {
    inflow_df <- parameters %>%
      mutate(!!!lazy_eval(x, data = .)) %>%
      .[c("model_time", "state_time", to_keep)] %>%
      reshape2::melt(
        id.vars = c("model_time", "state_time"),
        variable.name = ".state",
        value.name = ".value"
      ) %>%
      mutate(
        .full_state = as.character(.state),
        .state = as.character(.state)
      )
  }
  
  
  stopifnot(
    all(inflow_df$.value >= 0),
    all(!is.na(inflow_df$.value))
  )
  
  all_state_names <- unique(inflow_df$.full_state)
  inflow_mat <- inflow_df %>%
    reshape2::acast(
      model_time ~ factor(.full_state, levels = all_state_names),
      value.var = ".value",
      fill = 0
    )
  
  tibble::as_tibble(inflow_mat)
  
}


safe_eval <- function(x, .dots, .vartype = "parameter") {
  
  if (.vartype == "parameter") {
    expression_text = "parameter"
  } else if (.vartype == "init") {
    expression_text = "initial probability for state"
  } else if (.vartype == "value") {
    expression_text = "value"
  } else {
    expression_text = .vartype
  }
  n_par <- length(.dots)
  par_names <- names(.dots)
  res <- x
  for(i in seq_len(n_par)) {
    par_res <- try(lazy_eval(.dots[[i]], data = res), silent = T)
    par_name <- par_names[i]
    if (inherits(par_res, "try-error")) {
      
      # Pull of the lazyeval part of error call
      if (startsWith(par_res, "Error in eval(x$expr, data, x$env) : ")) {
        par_res <- sub(
          "Error in eval(x$expr, data, x$env) : ",
          "",
          par_res,
          fixed = T
        )
      }
      
      # Check if binding not found
      if (startsWith(par_res, "Binding not found: ")) {
        par_res <- paste0(
          sub(
            "Binding not found: ",
            "reference to undefined variable '",
            substr(par_res, 0, stop = nchar(par_res) - 2),
            fixed = T
          ),
          "'."
        )
      }
      
      stop(sprintf(
        "Error in %s '%s', %s", expression_text, par_name, par_res),
        call. = FALSE)
    }
    res[[par_name]] <- par_res
  }
  
  if ((use_fn <- options()$heRomod.inf_parameter) != "ignore") {
    
    if (any(these_are_inf <- sapply(res, is.infinite))) {
      inf_param_nums <- unique(which(these_are_inf, arr.ind = TRUE)[,2])
      inf_param_names <- names(res)[inf_param_nums]
      
      error_message <- paste(
        "Infinite parameter values:",
        paste(inf_param_names, collapse = ", "),
        ";\n",
        "See the option heRomod.inf_parameter, which",
        "can be 'ignore', 'warning', or 'stop' (the default)."
      )
      get(use_fn)(error_message)
    }
  }
  
  # Check for missing values
  if (any(is.na(res))) {
    index <- which(apply(res, 2, function(x) any(is.na(x)))==T)[1]
    param_name <- colnames(res)[index]
    text_error <- 'Calculation resulted in missing values.'
    stop(sprintf(
        "Error in %s '%s', %s", expression_text, param_name, text_error),
        call. = FALSE)
  }
  
  res
}
