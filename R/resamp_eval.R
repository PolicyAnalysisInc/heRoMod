#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#* Modified work Copyright (C) 2017  Matt Wiener
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


#' Run Probabilistic Uncertainty Analysis
#' 
#' @param model The result of [run_model()].
#' @param psa Resampling distribution for parameters defined
#'   by [define_psa()].
#' @param resample Deprecated. Resampling distribution for
#'   parameters defined by [define_psa()].
#' @param N > 0. Number of simulation to run.
#'   
#' @return A list with one `data.frame` per model.
#' @export
#' 
#' @example inst/examples/example_run_psa.R
#'   
run_psa <- function(model, psa, N, resample, cores = 1, report_progress = identity) {
  if (! missing(resample)) {
    warning("Argument 'resample' is deprecated, use 'psa' instead.")
    psa <- resample
  }
  
  stopifnot(
    N > 0,
    ! is.null(N)
  )
  
  if (! all(c(".cost", ".effect") %in% names(get_model_results(model)))) {
    stop("No cost and/or effect defined, probabilistic analysis unavailable.")
  }
  
  newdata <- eval_resample(psa, N, model)
  
  list_res <- list()
  for (n in get_strategy_names(model)) {
    message(sprintf("Resampling strategy '%s'...", n))
    model_res <- eval_strategy_newdata(
      x = model,
      strategy = n,
      newdata = newdata,
      cores = cores,
      report_progress
    )
    list_res <- c(
      list_res,
      list(
        model_res %>% 
          rowwise() %>% 
          do(get_total_state_values(.$.mod)) %>% 
          bind_cols(newdata) %>% 
          ungroup() %>%
          mutate(.group_weight = map_dbl(model_res$.mod, function(x) {
            if('.group_weight' %in% colnames(x$parameters)) {
              x$parameters$.group_weight[1]
            } else {
              NA
            }
          }))
      )
    )
  }
  
  names(list_res) <- get_strategy_names(model)
  index <- seq_len(N)
  
  for (n in names(list_res)) {
    list_res[[n]]$.strategy_names <- n
    list_res[[n]]$.index <- index
  }
  
  res <- 
    bind_rows(list_res)
  res <- res %>% mutate(!!!lazy_eval(get_ce(model), data = .))
  
  run_model <- res %>% 
    select(-.index) %>% 
    group_by(.strategy_names) %>%
    summarise_all(mean) %>% 
    as.data.frame()
  
  structure(
    list(
      psa = res,
      run_model = run_model[names(get_model_results(model))],
      model = model,
      N = N,
      resamp_par = names(newdata)
    ),
    class = c("psa", class(list()))
  )
}

get_model <- function(x) {
  UseMethod("get_model")
}

get_model.psa <- function(x) {
  x$model
}

get_model_results.psa <- function(x) {
  x$run_model
}

get_cycles.psa <- function(x) {
  get_cycles(get_model(x))
}

get_uneval_init.psa <- function(x) {
  get_uneval_init(get_model(x))
}

get_method.psa <- function(x) {
  get_method(get_model(x))
}

get_central_strategy.psa <- function(x, ...) {
  get_central_strategy(get_model(x))
}

get_noncomparable_strategy.psa <- function(x, ...) {
  get_noncomparable_strategy(summary(x, ...)$res_comp)
}

get_root_strategy.psa <- function(x, ...) {
  get_root_strategy(summary(x, ...)$res_comp)
}

eval_correlation <- function(x, var_names) {
  res <- diag(length(var_names))
  colnames(res) <- var_names
  rownames(res) <- var_names
  
  for (i in seq_len(nrow(x))) {
    res[x$v1[i], x$v2[i]] <- x$cor[i]
    res[x$v2[i], x$v1[i]] <- x$cor[i]
  }
  res
}

#' Evaluate Resampling Definition
#' 
#' @param psa A [define_psa()] object.
#' @param N > 0. Number of simulation to run.
#'   
#' @return A `data.frame` of resampled values with on 
#'   column per parameter and `N` rows.
#'   
#' @keywords internal
eval_resample <- function(psa, N, model = NULL) {
  
  base_env <- new.env(parent = asNamespace("heRomod"))
  list_qdist <- lapply(
    psa$list_qdist,
    function(x) {
      the_env <- new.env(parent = base_env)
      old_env <- as.list(environment(x))
      if(!is.null(model)) {
        var_name <- as.character(lhs(x))
        the_env$bc <- model$eval_strategy_list[[1]]$parameters[[var_name]][1]
      }
      eval(rhs(x), envir = the_env)
    }) %>% 
    unlist(recursive = FALSE)
  
  lapply(list_qdist, function(x) {
    if (! inherits(x, "function")) {
      stop("Distributions must be defined as functions.")
    }
  })
  
  names(list_qdist) <- unlist(
    lapply(
      psa$list_qdist,
      function(x) all.vars(lhs(x))
    )
  )
  
  mat_p <- stats::pnorm(
    mvnfast::rmvn(
      n = N,
      mu = rep(0, length(list_qdist)),
      sigma = psa$correlation
    )
  )
  
  list_res <- mapply(
    function(i, f) f(mat_p[, i]),
    seq_len(ncol(mat_p)),
    list_qdist
  )
  
  if (length(dim(list_res)) < 2) {
    list_res <- matrix(list_res, ncol = length(list_res))
  }
  
  colnames(list_res) <- names(list_qdist)
  res <- as.data.frame(list_res)
  
  for (m in psa$multinom) {
    call_denom <- make_call(m, "+")
    list_expr <- as.lazy_dots(
      c(list(
        .denom = call_denom),
      stats::setNames(
        lapply(
          m,
          function(x) as.call(list(as.name("/"), as.name(x), as.name(".denom")))),
        m)))
    res <- res %>%
      mutate(.denom = !!call_denom) %>% 
      mutate(!!!lazy_eval(list_expr, data = .)) %>% 
      select(- .denom)
  }
  na_cols <- apply(res, 2, function(x) any(is.na(x)))
  if (any(na_cols)) {
    err_cols <- colnames(res)[na_cols]
    err_msg <- paste0(
      'Sampling distribution resulted in missing values for variables: ',
      paste(err_cols, collapse = ', '),
      '.'
    )
    stop(err_msg, call. = F)
  }
  res
}
