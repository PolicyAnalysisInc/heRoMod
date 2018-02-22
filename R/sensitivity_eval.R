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

#' Run Sensitivity Analysis
#' 
#' @param model An evaluated Markov model.
#' @param dsa An object returned by 
#'   [define_dsa()].
#' @return A `data.frame` with one row per model and 
#'   parameter value.
#' @export
#' 
#' @example inst/examples/example_run_dsa.R
run_dsa <- function(model, dsa) UseMethod("run_dsa")

#' @export
run_dsa.run_model <- function(model, dsa) {
  
  if (! all(c(".cost", ".effect") %in% names(get_model_results(model)))) {
    stop("No cost and/or effect defined, sensitivity analysis unavailable.")
  }
  
  init <- get_uneval_init(model)
  cycles <- get_cycles(model)
  method <- get_method(model)
  strategy_names <- get_strategy_names(model)
  
  n_par <- length(dsa$variables)
  pos_par <- cumsum(c(1, rep(c(n_par, n_par+1), n_par)))
  pos_par <- pos_par[-length(pos_par)]
  
  list_res <- list()
  e_newdata <- list()
  for (n in strategy_names) {
    message(sprintf(
      "Running DSA on strategy '%s'...", n
    ))
    
    n_scenario <- nrow(dsa$dsa)
    n_var <- ncol(dsa$dsa)
    var_names <- colnames(dsa$dsa)
    bc_param <- model$eval_strategy_list[[n]]$parameters
    dsa_table <- as.data.frame(dsa$dsa)
    for (i in seq_len(n_var)) {
      dsa_table[[i]] <- lapply(seq_len(n_scenario), function(j) {
        lazy_param <- dsa$dsa[[i]][[j]]
        if ("lazy" %in% class(dsa$dsa[[i]][[j]])) {
          lazy_param$env <- new.env(parent = dsa$dsa[[i]][[j]]$env)
          lazy_param$env$bc <- bc_param[[var_names[i]]]
        }
        lazy_param
      })
    }
    
    tab <- eval_strategy_newdata(
      model,
      strategy = n,
      newdata = dsa_table
    )
    
    res <- tab %>% 
      dplyr::mutate_if(
        names(tab) %in% dsa$variables,
        dplyr::funs(to_text_dots),
        name = FALSE
      )
    
    list_res <- c(
      list_res,
      list(res)
    )
    
    e_newdata <- c(
      e_newdata,
      list(unlist(lapply(
        tab$.mod,
        function(x) x$parameters[1, dsa$variables]))[pos_par]))
    
    names(e_newdata)[length(e_newdata)] <- n
  }
  
  for (i in seq_along(strategy_names)) {
    list_res[[i]]$.strategy_names <- strategy_names[i]
  }

  res <- 
    dplyr::bind_rows(list_res) %>%
    reshape_long(
      key_col = ".par_names", value_col = ".par_value",
      gather_cols = dsa$variables, na.rm = TRUE) %>% 
    dplyr::rowwise()
  
  add_newdata <- function(df) {
    df$.par_value_eval <- unlist(e_newdata)
    return(df)
  }
  
  res <- res %>% 
    dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
    dplyr::bind_cols(res %>% dplyr::select_(~ - .mod)) %>% 
    dplyr::ungroup() %>% 
    dplyr::do(add_newdata(.)) %>% 
    dplyr::mutate_(
      .dots = get_ce(model))
  
  structure(
    list(
      dsa = res,
      variables = dsa$variables,
      model = model
    ),
    class = c("dsa", "list")
  )
}

#' @export
run_dsa.updated_model <- function(model, dsa) {
  # n_groups <- length(model$model_list[[1]]$.mod)
  # group_models <- lapply(model$model_list, function(x) {
  # 
  # })
  
  if (! all(c(".cost", ".effect") %in% names(get_model_results(model$model)))) {
    stop("No cost and/or effect defined, sensitivity analysis unavailable.")
  }
  
  init <- get_uneval_init(model$model)
  cycles <- get_cycles(mode$modell)
  method <- get_method(model$model)
  strategy_names <- get_strategy_names(model$model)
  
  n_par <- length(dsa$variables)
  pos_par <- cumsum(c(1, rep(c(n_par, n_par+1), n_par)))
  pos_par <- pos_par[-length(pos_par)]
  
  list_res <- list()
  e_newdata <- list()
  for (n in strategy_names) {
    message(sprintf(
      "Running DSA on strategy '%s'...", n
    ))
    tab <- eval_strategy_newdata(
      model,
      strategy = n,
      newdata = dsa$dsa
    )
    
    res <- tab %>% 
      dplyr::mutate_if(
        names(tab) %in% dsa$variables,
        dplyr::funs(to_text_dots),
        name = FALSE
      )
    
    list_res <- c(
      list_res,
      list(res)
    )
    
    e_newdata <- c(
      e_newdata,
      list(unlist(lapply(
        tab$.mod,
        function(x) x$parameters[1, dsa$variables]))[pos_par]))
    
    names(e_newdata)[length(e_newdata)] <- n
  }
  
  for (i in seq_along(strategy_names)) {
    list_res[[i]]$.strategy_names <- strategy_names[i]
  }
  
  res <- 
    dplyr::bind_rows(list_res) %>%
    reshape_long(
      key_col = ".par_names", value_col = ".par_value",
      gather_cols = dsa$variables, na.rm = TRUE) %>% 
    dplyr::rowwise()
  
  add_newdata <- function(df) {
    df$.par_value_eval <- unlist(e_newdata)
    return(df)
  }
  
  res <- res %>% 
    dplyr::do_(~ get_total_state_values(.$.mod)) %>% 
    dplyr::bind_cols(res %>% dplyr::select_(~ - .mod)) %>% 
    dplyr::ungroup() %>% 
    dplyr::do(add_newdata(.)) %>% 
    dplyr::mutate_(
      .dots = get_ce(model))
  
  structure(
    list(
      dsa = res,
      variables = dsa$variables,
      model = model
    ),
    class = c("dsa", "list")
  )
}

get_model.dsa <- function(x) {
  x$model
}

digits_at_diff <- function(x, y, addl_digits = 1){
  stopifnot(length(x) == length(y))
  diff <- abs(x - y)
  num_digits <- -floor(log(diff, 10)) + addl_digits
  round_x <- 
    sapply(seq(along = x), 
           function(i){round(x[i], num_digits[i])})
  round_y <- 
    sapply(seq(along = y), 
           function(i){round(y[i], num_digits[i])})
  list(x = round_x, y = round_y, nd = num_digits)
}
