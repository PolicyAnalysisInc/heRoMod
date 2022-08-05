eval_sparse_matrix <- function(x, parameters, expand = NULL, state_groups = NULL) {
  
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
  
  
  n_cycles <- length(unique(parameters$markov_cycle))
  n_full_state <- nrow(expand)
  trans_matrix <- array(0, c(n_cycles, n_full_state, n_full_state))
  
  trans_table <- eval_matrix_table(x, parameters, expand, state_groups) %>%
    replace_C()

  # Make sure that values are numeric, or integer which would be odd but would technically be valid
  # if all transition probabilities are 1 or 0.
  matrix_type <- class(trans_table$.value)
  if (!matrix_type %in% c('numeric', 'integer')) {
    stop(sprintf(
      "Error in transition matrix, values for transition probabilities are of type '%s', should be of type 'numeric'.", matrix_type),
      call. = FALSE)
  }
  
  check_matrix(trans_table)
  
  # split into list of sparse matrices
  matrices <- trans_table %>%
    group_by(model_time) %>%
    group_split %>%
    map(function(matrix_tbl) {
      sparse <- sparseMatrix(
        matrix_tbl$.from_e_i,
        matrix_tbl$.to_e_i,
        x = matrix_tbl$.value,
        dims = c(n_full_state, n_full_state)
      )
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

check_matrix.data.frame <- function(x) {
  
  
  correct_sum <- is_zero((max(x$.from_e_i) * max(x$model_time)) - sum(x$.value))
  outside_range <- any(!is_zero(pmin(x$.value, 0))) || any(!is_zero(pmax(x$.value, 1) - 1))
  
  if (!correct_sum || outside_range) {
  
    sums <- x %>%
      group_by(model_time, .from_e) %>%
      summarize(sum = sum(.value)) %>%
      ungroup()
    
    problem_rows <- sums %>%
      filter(!is_zero(sum - 1)) %>%
      group_by(.from_e) %>%
      group_split() %>%
      map(function(x) {
        data.frame(state = x$.from_e[1], cycles = to_number_list_string(x$model_time), stringsAsFactors=F)
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
      filter(is_equal(pmax(abs(.value-0.5), 0.5), 0.5)) %>%
      group_by(.from_e, .to_e) %>%
      group_split() %>%
      map(function(x) {
        data.frame(from = x$.from_e[1], to = x$.to_e[1], cycles = to_number_list_string(x$model_time), stringsAsFactors=F)
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
}

replace_C.data.frame <- function(x, state_names) {
  res <- as.data.table(x) %>%
    .[,.is_complement := .value == -pi] %>%
    .[,c('.n_complement', '.all_else') := list(sum(.is_complement), sum(.value)), by=list(model_time, .from_e_i)] %>%
    .[,.value:=if_else(.is_complement, ifelse(is_zero(.all_else), 0, 1 - .all_else - pi), .value)]
  
  # Make sure C is used only once per state per cycle
  if (any(res$.n_complement > 1)) {
    problem_rows <- res %>%
      filter(.n_complement > 1) %>%
      group_by(.from_e) %>%
      group_split() %>%
      map(function(x) {
        data.frame(state = x$.from_e[1], cycles = to_number_list_string(x$cycle), stringsAsFactors=F)
      }) %>%
      bind_rows()
    message <- paste0(
      'Error in transition matrix, keyword "C" used more than once per state:\n\n',
      paste(capture.output(print(problem_rows, row.names = F)), collapse = "\n")
    )
    stop(message, call. = F)
  }
  
  res
}