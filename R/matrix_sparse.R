eval_sparse_matrix <- function(x, parameters, expand = NULL) {
  
  # Assinging NULLS to avoid CMD Check issues
  .from <- .to <- .limit <- .from_lim <- .to_state_time <- NULL
  .to_state_time <- .value <- .state <- state_time <- .full_state <- NULL
  
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
  
  nrow_param = nrow(parameters)
  matrix_pos_names <- names(x)
  state_trans_names <- paste0(
    rep(state_names, each = length(state_names)),
    ' \u2192 ',
    rep(state_names, length(state_names))
  )
  names(x) <- state_trans_names
  renamer <- state_trans_names
  names(renamer) <- matrix_pos_names
  eval_trans_probs <- safe_eval(parameters, .dots = x, .vartype = "transition") %>%
    rename(!!!syms(renamer))
  names(x) <- matrix_pos_names
  
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
      .from_e_i = as.numeric(factor(.from_e, levels = expand$.full_state)),
      .to_e_i = as.numeric(factor(.to_e, levels = expand$.full_state)),
      .cycle = as.numeric(factor(model_time, levels = sort(unique(model_time))))
    )
  
  trans_table <- trans_table %>%
    mutate(.is_complement = .value == -pi) %>%
    group_by(model_time, .from_e_i) %>%
    mutate(
      .n_complement = sum(.is_complement),
      .complement = 1 - sum(.value) - pi
    ) %>%
    ungroup() %>%
    mutate(
      .value = if_else(.is_complement, .complement, .value)
    )
  
  # Make sure C is used only once per state per cycle
  if (any(trans_table$.n_complement > 1)) {
    problem_rows <- trans_table %>%
      filter(.n_complement > 1) %>%
      group_by(.from_e) %>%
      group_split() %>%
      map(function(x) {
        first_cycle <- min(x$model_time)
        last_cycle <- max(x$model_time)
        if (first_cycle == last_cycle) {
          cycles = as.character(first_cycle)
        } else if (all(x$model_time == seq(from = first_cycle, to = last_cycle, by = 1))) {
          cycles = paste0(first_cycle, "-", last_cycle)
        } else {
          cycles = paste(x$cycle,collapse=",")
        }
        data.frame(state = x$.from_e[1], cycles = cycles, stringsAsFactors=F)
      }) %>%
      bind_rows()
    message <- paste0(
      'Error in transition matrix, keyword "C" used more than once per state:\n\n',
      paste(capture.output(print(problem_rows, row.names = F)), collapse = "\n")
    )
    stop(message, call. = F)
  }
  
  # Make sure that values are numeric, or integer which would be odd but would technically be valid
  # if all transition probabilities are 1 or 0.
  matrix_type <- class(trans_table$.value)
  if (!matrix_type %in% c('numeric', 'integer')) {
    stop(sprintf(
      "Error in transition matrix, values for transition probabilities are of type '%s', should be of type 'numeric'.", matrix_type),
      call. = FALSE)
  }
  
  check_sparse_matrix(trans_table)
  
  # split into list of sparse matrices
  matrices <- trans_table %>%
    group_by(model_time) %>%
    group_split %>%
    map(function(matrix_tbl) {
      sparse <- sparseMatrix(matrix_tbl$.from_e_i, matrix_tbl$.to_e_i, x = matrix_tbl$.value)
      rownames(sparse) <- expand$.full_state
      colnames(sparse) <- expand$.full_state
      sparse
    }) %>%
    set_names(seq_len(length(.)))
  
  structure(
    matrices,
    class = c("eval_sparse_matrix", "eval_matrix", "list"),
    state_names = expand$.full_state,
    entry = expand$state_time == 1
  )
}

check_sparse_matrix <- function(x) {
  
  sums <- x %>%
    group_by(model_time, .from_e) %>%
    summarize(sum = sum(.value)) %>%
    ungroup()
  
  problem_rows <- sums %>%
    filter(!is_zero(sum - 1)) %>%
    group_by(.from_e) %>%
    group_split() %>%
    map(function(x) {
      first_cycle <- min(x$model_time)
      last_cycle <- max(x$model_time)
      if (first_cycle == last_cycle) {
        cycles = as.character(first_cycle)
      } else if (all(x$model_time == seq(from = first_cycle, to = last_cycle, by = 1))) {
        cycles = paste0(first_cycle, "-", last_cycle)
      } else {
        cycles = paste(x$cycle,collapse=",")
      }
      data.frame(state = x$.from_e[1], cycles = cycles, stringsAsFactors=F)
    }) %>%
    bind_rows()
  
  if (nrow(problem_rows) > 0) {
    stop(
      paste0(
        "Not all transition matrix rows sum to 1.\n\n",
        paste(capture.output(print(problem_rows, row.names = F)), collapse = "\n")
      ),
      call. = F
    )
  }
  
  problem_rows <- x %>%
    filter(abs(.value - 0.5) > 0.5) %>%
    group_by(.from_e, .to_e) %>%
    group_split() %>%
    map(function(x) {
      first_cycle <- min(x$model_time)
      last_cycle <- max(x$model_time)
      if (first_cycle == last_cycle) {
        cycles = as.character(first_cycle)
      } else if (all(x$model_time == seq(from = first_cycle, to = last_cycle, by = 1))) {
        cycles = paste0(first_cycle, "-", last_cycle)
      } else {
        cycles = paste(x$cycle,collapse=",")
      }
      data.frame(from = x$.from_e[1], to = x$.to_e[1], cycles = cycles, stringsAsFactors=F)
    }) %>%
    bind_rows()
  
  if (nrow(problem_rows) > 0) {
    stop(paste0(
      "Some transition probabilities are outside the interval [0 - 1]:\n\n",
      paste(capture.output(print(problem_rows, row.names = F)), collapse = "\n")
    ),
    call. = F)
    
  }
}