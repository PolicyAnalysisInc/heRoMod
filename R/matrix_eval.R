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
  UseMethod('check_matrix', x)
}

check_matrix.array <- function(x) {
  stopifnot(length(dim(x)) == 3)
  
  if (! isTRUE(all.equal(
    range(rowSums(x, dims = 2)),
    c(1, 1)))) {
    problem_rows <- which(rowSums(x, dims = 2) != 1, arr.ind = TRUE)
    state_names <- get_state_names(x)
    problems <- data.frame(
      cycle = problem_rows[,1], 
      state = state_names[problem_rows[,2]]) %>%
      plyr::ddply("state", function(x) {
        data.frame(cycles = to_number_list_string(x$cycle))
      })
    
    stop(
      paste0(
        "Not all transition matrix rows sum to 1.\n\n",
        paste(capture.output(print(problems, row.names = F)), collapse = "\n")
      ),
      call. = F
    )
    
    
  }
  
  problem_indices <- !is_equal_mat(pmax(abs(x-0.5), 0.5), 0.5)
  if (any(problem_indices)) {
    problem <- which(problem_indices, arr.ind = TRUE)
    problem <- tibble::as_tibble(problem)
    names(problem) <- c("cycle", "from", "to")
    states <- get_state_names(x)
    problem$from <- states[problem$from]
    problem$to <- states[problem$to]
    problem %>%
      group_by(from, to) %>%
      group_split() %>%
      map(function(x) {
        data.frame(from = x$from, to = x$to, cycles = to_number_list_string(x$cycle), stringsAsFactors=F)
      }) %>%
      bind_rows()
    
    if (nrow(problem) > 0) {
      stop(paste0(
        "Some transition probabilities are outside the interval [0 - 1]:\n\n",
        paste(capture.output(print(as.data.frame(problem), row.names = F)), collapse = "\n")
      ),
      call. = F)
    }
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

eval_transition.uneval_matrix <- function(x, parameters, expand = NULL, state_groups = NULL) {

  
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
  
  # Use sparse matrix evaluation if more than 50 states
  # as it is faster and more memory-efficient
  if (nrow(expand) > 50) {
    eval_sparse_matrix(x, parameters, expand, state_groups)
  } else {
    eval_matrix(x, parameters, expand, state_groups)
  }
}


eval_matrix <- function(x, parameters, expand = NULL, state_groups = NULL) {
  
  # update calls to dispatch_strategy()
  x <- dispatch_strategy_hack(x)
  x <- by_group_hack(x)
  
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
  
  trans_table <- eval_matrix_table(x, parameters, expand, state_groups)
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
    state_names = dimnames(trans_matrix)[[2]],
    entry = expand$state_time == 1
  )
}

eval_matrix_table <- function(x, parameters, expand, state_groups) {
  
  n_state <- sqrt(length(x))
  state_names <- attr(x, "state_names")
  n_cycles <- length(unique(parameters$markov_cycle))
  n_full_state <- nrow(expand)
  
  if(any(expand$.expand)) {
    
    nrow_param = nrow(parameters)
    matrix_pos_names <- names(x)
    state_trans_names <- paste0(
      rep(state_names, each = length(state_names)),
      ' \u2192 ',
      rep(state_names, length(state_names))
    )
    names(x) <- state_trans_names
    indices <- map_lgl(x, function(y) as.character(y)[1] != '0')
    .nz_trans_guide <- strsplit(state_trans_names[indices], ' → ', fixed = T) %>%
      map_dfr(function(z) tibble(.from = z[[1]], .to = z[[2]])) %>%
      mutate(.trans = state_trans_names[indices])

    trans_table <- safe_eval(parameters, .dots = x[indices], .vartype = "transition") %>%
      .[c('state_time', 'model_time', names(x)[indices])] %>%
      data.table::as.data.table() %>%
      data.table::melt(
        id.vars = c('model_time', 'state_time'),
        measure.vars = names(x)[indices],
        variable.name = '.trans',
        value.name = '.value'
      ) %>%
      left_join(.nz_trans_guide, by = '.trans') %>%
      select(-.trans) %>%
      filter(.value != 0) %>%
      left_join(
        rename(state_groups, .from_state_group = group),
        by = c(.from = "name")
      ) %>%
      left_join(
        rename(state_groups, .to_state_group = group, .share = share),
        by = c(.to = "name")
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
        .to_state_time = ifelse(.from == .to | (.from_state_group == .to_state_group & .share), pmin(state_time + 1, .from_lim), 1)
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
        .from_e_i = as.numeric(factor(.from_e, levels = expand$.full_state)),
        .to_e_i = as.numeric(factor(.to_e, levels = expand$.full_state)),
        .cycle = as.numeric(factor(model_time, levels = sort(unique(model_time)))),
        .index = .cycle + (.from_e_i - 1) * n_cycles + ((.to_e_i - 1) * n_cycles * n_full_state)
      )
  } else {
    parameters <- filter(parameters, state_time == 1)
    nrow_param = nrow(parameters)
    matrix_pos_names <- names(x)
    state_trans_names <- paste0(
      rep(state_names, each = length(state_names)),
      ' \u2192 ',
      rep(state_names, length(state_names))
    )
    names(x) <- state_trans_names
    indices <- map_lgl(x, function(y) as.character(y)[1] != '0')
    .nz_trans_guide <- strsplit(state_trans_names[indices], ' → ', fixed = T) %>%
      map_dfr(function(z) tibble(.from = z[[1]], .to = z[[2]])) %>%
      mutate(.trans = state_trans_names[indices])
    
    trans_table <- safe_eval(parameters, .dots = x[indices], .vartype = "transition") %>%
      .[c('state_time', 'model_time', names(x)[indices])] %>%
      data.table::as.data.table() %>%
      data.table::melt(
        id.vars = c('model_time', 'state_time'),
        measure.vars = names(x)[indices],
        variable.name = '.trans',
        value.name = '.value'
      ) %>%
      left_join(.nz_trans_guide, by = '.trans') %>%
      select(-.trans) %>%
      mutate(
        .from_e = .from,
        .to_e = .to,
        .from_e_i = as.numeric(factor(.from, levels = expand$.full_state)),
        .to_e_i = as.numeric(factor(.to, levels = expand$.full_state)),
        .cycle = as.numeric(factor(model_time, levels = sort(unique(model_time)))),
        .index = .cycle + (.from_e_i - 1) * n_cycles + ((.to_e_i - 1) * n_cycles * n_full_state)
      )
  }
  
  trans_table
}

split_array <- function(a) {
  iter <- dim(a)[1]
  the_list <- vector(iter, mode = "list")
  for(i in seq_len(iter)) {
    the_list[[i]] <- matrix(a[i,,,drop = F], ncol = dim(a)[2], dim(a)[3])
    colnames(the_list[[i]]) <- dimnames(a)[[3]]
    rownames(the_list[[i]]) <- dimnames(a)[[2]]
  }
  names(the_list) <- dimnames(a)[[1]]
  return(the_list)
}

replace_C <- function(x, state_names) {
  UseMethod('replace_C', x)
}

replace_C.array <- function(x, state_names) {
  posC <- x == -pi
  
  c_counts <- rowSums(posC, dims = 2)
  colnames(c_counts) <- state_names
  if (!all(c_counts <= 1)) {
    problem_states <- c_counts[, as.logical(apply(c_counts, 2, function(z) any(z > 1))), drop = F]
    problems <- lapply(seq_len(ncol(problem_states)), function(i) {
      cycles <- problem_states[ , i]
      problem_cycles <- which(cycles > 1)
      data.frame(
        state = colnames(problem_states)[i],
        cycles = to_number_list_string(problem_cycles),
        stringsAsFactors = F
      )
    }) %>%
      bind_rows() %>%
      as.data.frame()
    
    message <- paste0(
      'Error in transition matrix, keyword "C" used more than once per state:\n\n',
      paste(capture.output(print(problems, row.names = F)), collapse = "\n")
    )
    stop(message, call. = F)
  }
  
  x[posC] <- 0
  
  valC <- 1 - rowSums(x, dims = 2)[which(posC, arr.ind = TRUE)[, -3]]
  
  # Sometimes the 1 - sum(trans_probs) is equal to a negative value arbitrarily close to
  # zero due to floating point wierdness. In these cases, we should use zero as the complementary
  # probability because otherwise the matrix will be treated as invalid because the completementary
  # probability is negtive.
  near_zero <- is_zero(valC)
  neg <- valC < 0
  valC[near_zero & neg] <- 0
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
