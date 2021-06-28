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


#' Iteratively Evaluate a Markov Model With New Parameter 
#' Values
#' 
#' Given a data.frame with on set of new parameters values 
#' per row, iteratively evaluate the model over the set of 
#' new values.
#' 
#' New parameters with a missing value (`NA`) do not 
#' replace existing parameters.
#' 
#' @param x Result from [run_model()].
#' @param model Name or index of model to recompute.
#' @param newdata a data.frame whose names match parameters 
#'   names. `model` will be evaluated iteratively, 
#'   taking successive values from each row.
#'   
#' @return A data.frame containing the values of 
#'   `newdata` and each Markov Model evaluation in 
#'   `res`.
#'   
#' @example inst/examples/example_eval_strategy_newdata.R
#'   
#' @keywords internal
eval_strategy_newdata <- function(x, strategy = 1, newdata, cores = 1, report_progress = identity) {
  if (is.null(cores)) cores <- 1
  strategy <- check_strategy_index(x = x, i = strategy)
  
  cycles <- get_cycles(x)
  init <- get_uneval_init(x)
  inflow <- get_inflow(x)
  method <- get_method(x)
  disc_method <- x$disc_method
  old_parameters <- get_parameters(x)
  aux_params <- x$aux_params
  uneval_strategy <- x$uneval_strategy_list[[strategy]]
  expand_limit <- get_expand_limit(x, strategy)
  
  newdata <- newdata %>%
    dplyr::mutate(.iteration = seq_len(n()))
  pnewdata <- split(newdata, newdata$.iteration)
  
  log_info(paste("Using a cluster with", cores, "cores."))
  future::plan(future::multisession, workers = cores)
  suppressMessages(
    pieces <- #parallel::mclapply(pnewdata, function(newdata) {
      furrr::future_map(pnewdata, function(newdata) {
      #lapply(pnewdata, function(newdata) {
      newdata %>% 
        rowwise() %>% 
        do({
          df <- as_tibble(
            lapply(., function(x) if(class(x) == 'lazy') list(x) else x)
          )
          iter <- df$.iteration
          tibble(
          .mod = list(try(eval_newdata(
            as.data.frame(df),
            strategy = uneval_strategy,
            old_parameters = old_parameters,
            aux_params = aux_params,
            cycles = cycles,
            init = init,
            inflow = inflow,
            method = method,
            strategy_name = strategy,
            expand_limit = expand_limit,
            disc_method = disc_method,
            iteration = iter,
            report_progress = report_progress
          )
          )))}) %>%
        ungroup() %>% 
        bind_cols(
          newdata
        )
    })
  )
  plyr::l_ply(
    pieces,
    function(x) {
      plyr::l_ply(x$.mod, function(y) {
        if ("try-error" %in% class(y)) {
          error_message <- clean_err_msg(y)
          log_error(error_message)
          stop(error_message, call. = F)
        }
      })
    }
  )
  res <- bind_rows(pieces)
  rownames(res) <- NULL
  res
}

eval_newdata <- function(new_parameters, strategy, old_parameters,
                         cycles, init, method, inflow,
                         strategy_name, expand_limit, aux_params = NULL,
                         disc_method = 'start', iteration = NULL, report_progress = identity) {
  new_parameters <- Filter(
    function(x) all(! is.na(x)),
    new_parameters
  )
  
  lazy_new_param <- to_dots(purrr::map(as.list(select(new_parameters, -.iteration)), ~.[[1]]))
  
  parameters <- utils::modifyList(
    old_parameters,
    lazy_new_param
  )
  res <- eval_strategy(
    strategy = strategy,
    parameters = parameters,
    cycles = cycles,
    init = init,
    method = method,
    inflow = inflow,
    strategy_name = strategy_name,
    expand_limit = expand_limit,
    aux_params = aux_params,
    disc_method = disc_method,
    report_progress = report_progress
  )
  res
}
