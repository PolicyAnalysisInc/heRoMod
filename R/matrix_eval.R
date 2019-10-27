#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#* Modified work Copyright (C) 2017  Matt Wiener
#* Modified work Copyright (C) 2017  Jordan Amdahl
#*
#* This program is free software: you can redistribute it and/or modify
#* it under the terms of the GNU General Public License as published by
#* the Free Software Foundation, either version 3 of the License, or
#* (at your option) any later version.
#*
#* This program is distributed in the hope that it will be useful,
#* but WITHOUT ANY WARRANTY; without even the implied warranty of
#* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#* GNU General Public License for more details.
#*
#* You should have received a copy of the GNU General Public License
#* along with this program.  If not, see <http://www.gnu.org/licenses/>.
#**************************************************************************


#' Check Markov Model Transition Matrix
#' 
#' Check whether a matrix fullfills the conditions to be a 
#' transition matrix.
#' 
#' This function is called by [eval_transition()]
#' and should not be used directly.
#' 
#' Checks whether all rows sum to 1 and all probabilities 
#' are between 0 and 1.
#' 
#' @param x a matrix.
#'   
#' @return `NULL`
#'   
#' @keywords internal
check_matrix <- function(x) {
  stopifnot(inherits(x, "array"))
  stopifnot(length(dim(x)) == 3)
  
  if (! isTRUE(all.equal(
    range(rowSums(x, dims = 2)),
    c(1, 1)))) {
    problem_rows <- which(rowSums(x, dims = 2) != 1, arr.ind = TRUE)
    state_names <- get_state_names(x)
    problem_rows <- data.frame(
      cycle = problem_rows[,1], 
      state = state_names[problem_rows[,2]]) %>%
      plyr::ddply("state", function(x) {
        first_cycle <- min(x$cycle)
        last_cycle <- max(x$cycle)
        if (all(x$cycles == seq(from = first_cycle, to = last_cycle, by = 1))) {
          cycles = paste0(first_cycle, "-", last_cycle)
        } else {
          cycles = paste(x$cycle,collapse=",")
        }
        data.frame(cycles = cycles)
      })
    
    stop(
      paste0(
        "Not all transition matrix rows sum to 1.\n",
        paste(capture.output(problem_rows), collapse = "\n")
      ),
      call. = F
    )
    
    
  }
  
  if (! sum(abs(x-0.5) > 0.5)==0) {
    problem <- which(x < 0 | x > 1, arr.ind = TRUE)
    # Use tibble here to avoid potentially confusing warnings about
    # Duplicate rownames
    problem <- tibble::as_tibble(problem)
    names(problem) <- c("cycle", "from", "to")
    states <- get_state_names(x)
    problem$from <- states[problem$from]
    problem$to <- states[problem$to]
    problem <- format.data.frame(problem, justify = "left")
    
    stop(sprintf(
      "Some transition probabilities are outside the interval [0 - 1]:\n%s",
      paste(sprintf(
        "cycle: %s, from: %s, to: %s",
        problem$cycle, problem$from, problem$to),
        collapse = "\n")
    ))
    
  }
}

#' Evaluate Markov Model Transition Matrix
#' 
#' Evaluate a transition matrix using evaluated parameters.
#' 
#' Runs checks on the transition matrix during evaluation.
#' 
#' This functions has been heavily optimized, and thus can
#' be difficult to read. Good luck...
#' 
#' @param x an `uneval_matrix` object.
#' @param parameters an `eval_parameters` object.
#' @param expand A tibble identifying which states
#'   should be expanded.
#'   
#' @return An `eval_matrix` object (actually a list of 
#'   transition matrices, one per cycle).
#'   
#' @keywords internal
eval_transition <- function(x, ...) {
  UseMethod("eval_transition")
}

eval_transition.uneval_matrix <- function(x, parameters, expand = NULL) {
  
  # Assinging NULLS to avoid CMD Check issues
  .from <- .to <- .limit <- .from_lim <- .to_state_time <- NULL
  .to_state_time <- .value <- .state <- state_time <- .full_state <- NULL
  
  # update calls to dispatch_strategy()
  x <- dispatch_strategy_hack(x)
  
  # Set up time values for which transition probabilities
  # will be evaluated
  time_values <- tibble::tibble(
    model_time = parameters$model_time
  )
  
  # Replace complement with negative pi
  parameters$C <- -pi
  
  # Get number of states + state names
  n_state <- sqrt(length(x))
  state_names <- attr(x, "state_names")
  
  # Fill in expansion table if empty
  if(is.null(expand)) {
    expand <- tibble::tibble(
      .state = state_names,
      .full_state = state_names,
      state_time = 1,
      .expand = F,
      .limit = 1
    )
  }
  
  expanding <- any(expand$.expand)
  
  
  
  n_cycles <- length(unique(parameters$markov_cycle))
  n_full_state <- nrow(expand)
  trans_matrix <- array(0, c(n_cycles, n_full_state, n_full_state))
  
  if(expanding) {
    
    nrow_param = nrow(parameters)
    eval_trans_probs <- safe_eval(parameters, .dots = x, .vartype = "transition")
    
    trans_table <- tibble::tibble(
      model_time = rep.int(parameters$model_time, times = n_state^2),
      state_time = rep.int(parameters$state_time, times = n_state^2),
      .from = rep(state_names, each = n_state * nrow_param),
      .to = rep(state_names, times = n_state, each = nrow_param),
      .value = as.numeric(as.matrix((eval_trans_probs[names(x)])))
    ) %>%
      left_join(
        transmute(
          expand,
          .state = .state,
          state_time = state_time,
          .from_e = .full_state,
          .from_lim = .limit
        ),
        by = c(".from" = ".state", "state_time" = "state_time")
      ) %>%
      filter(.from_lim >= state_time) %>%
      mutate(
        .to_state_time = ifelse(.from == .to, pmin(state_time + 1, .from_lim), 1)
      ) %>%
      left_join(
        transmute(
          expand,
          .state = .state,
          state_time = state_time,
          .to_e = .full_state
        ),
        by = c(".to" = ".state", ".to_state_time" = "state_time")
      ) %>%
      mutate(
        .from_e = as.numeric(factor(.from_e, levels = expand$.full_state)),
        .to_e = as.numeric(factor(.to_e, levels = expand$.full_state)),
        .cycle = as.numeric(factor(model_time, levels = sort(unique(model_time)))),
        .index = .cycle + (.from_e - 1) * n_cycles + ((.to_e - 1) * n_cycles * n_full_state)
      )
  } else {
    parameters <- filter(parameters, state_time == 1)
    nrow_param = nrow(parameters)
    eval_trans_probs <- safe_eval(parameters, .dots = x, .vartype = "transition")
    
    trans_table <- tibble::tibble(
      model_time = rep(parameters$model_time, times = n_state^2),
      state_time = rep(parameters$state_time, times = n_state^2),
      .from = rep(state_names, each = n_state * nrow_param),
      .to = rep(state_names, times = n_state, each = nrow_param),
      .value = unlist(eval_trans_probs[names(x)])
    ) %>%
      mutate(
        .from_e = as.numeric(factor(.from, levels = expand$.full_state)),
        .to_e = as.numeric(factor(.to, levels = expand$.full_state)),
        .cycle = as.numeric(factor(model_time, levels = sort(unique(model_time)))),
        .index = .cycle + (.from_e - 1) * n_cycles + ((.to_e - 1) * n_cycles * n_full_state)
      )
  }
  # Reshape into 3d matrix and calculate complements
  # trans_matrix <- trans_table %>%
  #   reshape2::acast(
  #     model_time ~
  #       factor(.from_e, levels = expand$.full_state) ~
  #       factor(.to_e, levels = expand$.full_state),
  #     value.var = ".value",
  #     fill = 0
  #   ) %>%
  #   replace_C
  trans_matrix <- array(0, c(n_cycles, n_full_state, n_full_state))
  trans_matrix[trans_table$.index] <- trans_table$.value
  
  # Make sure that the matrix is numeric
  matrix_type <- class(trans_matrix[1,1,])
  if (class(trans_matrix[1,1,]) != 'numeric') {
    stop(sprintf(
      "Error in transition matrix, values for transition probabilities are of type '%s', should be of type 'numeric'.", matrix_type),
      call. = FALSE)
  }
  trans_matrix <- replace_C(trans_matrix, expand$.full_state)
  dimnames(trans_matrix) <- list(
    seq_len(n_cycles),
    expand$.full_state,
    expand$.full_state
  )
  attr(trans_matrix, "state_names") <- expand$.full_state
  
  check_matrix(trans_matrix)
  
  structure(
    split_array(trans_matrix),
    class = c("eval_matrix", "list"),
    state_names = colnames(trans_matrix[1,,]),
    entry = expand$state_time == 1
  )
}

split_array <- function(a) {
  iter <- dim(a)[1]
  the_list <- vector(iter, mode = "list")
  for(i in seq_len(iter)) {
    the_list[[i]] <-a[i,,]
  }
  names(the_list) <- dimnames(a)[[1]]
  return(the_list)
}

replace_C <- function(x, state_names) {
  posC <- x == -pi
  
  c_counts <- rowSums(posC, dims = 2)
  colnames(c_counts) <- state_names
  if (!all(c_counts <= 1)) {
    problem_states <- c_counts[, apply(c_counts, 2, function(z) any(z > 1))]
    problems <- lapply(seq_len(ncol(problem_states)), function(i) {
      cycles <- problem_states[ , i]
      problem_cycles <- which(cycles > 1)
      min_cycle <- min(problem_cycles)
      max_cycle <- max(problem_cycles)
      if (all(problem_cycles == min_cycle:max_cycle)) {
        problem_cycles = paste0(min_cycle, '-', max_cycle)
      }
      data.frame(
        state = colnames(problem_states)[i],
        cycles = paste(problem_cycles, collapse = ', '),
        stringsAsFactors = F
      )
    }) %>%
      bind_rows() %>%
      as.data.frame()
    
    message <- paste0(
      'Error in transition matrix, keyword "C" used more than once per state:\n',
      paste(capture.output(problems), collapse = "\n")
    )
    stop(message, call. = F)
  }
  
  x[posC] <- 0
  
  valC <- 1 - rowSums(x, dims = 2)[which(posC, arr.ind = TRUE)[, -3]] 
  x[posC] <- valC
  x
}

get_state_names.eval_matrix <- function(x, ...){
  attr(x, "state_names")
}
get_state_names.array <- function(x, ...){
  attr(x, "state_names")
}

get_matrix_order.eval_matrix <- function(x){
  ncol(x[[1]])
}
