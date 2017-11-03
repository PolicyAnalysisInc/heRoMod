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
eval_strategy_newdata <- function(x, strategy = 1, newdata) {
  strategy <- check_strategy_index(x = x, i = strategy)
  
  cycles <- get_cycles(x)
  init <- get_uneval_init(x)
  inflow <- get_inflow(x)
  method <- get_method(x)
  old_parameters <- get_parameters(x)
  uneval_strategy <- x$uneval_strategy_list[[strategy]]
  expand_limit <- get_expand_limit(x, strategy)
  
  if (status_cluster(verbose = FALSE)) {
    cl <- get_cluster()
    
    num_cores <- length(cl)
    
    message(paste("Using a cluster with", num_cores, "cores."))
    
    split_vec <- rep(1:num_cores, each = nrow(newdata) %/% num_cores)
    split_vec <- c(split_vec, rep(num_cores, nrow(newdata) %% num_cores))
    
    pnewdata <- split(newdata, split_vec)
    parallel::clusterExport(
      cl, 
      c("uneval_strategy", "old_parameters", "pnewdata", 
        "cycles", "init", "method", "strategy"),
      envir = environment()
    )
    suppressMessages(
      pieces <- parallel::parLapply(cl, pnewdata, function(newdata) {
        newdata %>% 
          dplyr::rowwise() %>% 
          dplyr::do_(
            .mod = ~ eval_newdata(
              .,
              strategy = uneval_strategy,
              old_parameters = old_parameters,
              cycles = cycles,
              init = init,
              inflow = inflow,
              method = method,
              strategy_name = strategy,
              expand_limit = expand_limit
            )
          ) %>% 
          dplyr::ungroup() %>% 
          dplyr::bind_cols(
            newdata
          )
      })
    )
    res <- dplyr::bind_rows(pieces)
    rownames(res) <- NULL
    
  } else {
    suppressMessages(
      res <- newdata %>% 
        dplyr::rowwise() %>% 
        dplyr::do_(
          .mod = ~ eval_newdata(
            .,
            strategy = uneval_strategy,
            old_parameters = old_parameters,
            cycles = cycles,
            init = init,
            method = method,
            inflow = inflow,
            strategy_name = strategy,
            expand_limit = expand_limit
          )
        ) %>% 
        dplyr::ungroup() %>% 
        dplyr::bind_cols(
          newdata
        )
    )
  }
  res
}

eval_newdata <- function(new_parameters, strategy, old_parameters,
                         cycles, init, method, inflow,
                         strategy_name, expand_limit) {
  
  new_parameters <- Filter(
    function(x) all(! is.na(x)),
    new_parameters
  )
  
  lazy_new_param <- to_dots(new_parameters)
  
  parameters <- utils::modifyList(
    old_parameters,
    lazy_new_param
  )
  
  eval_strategy(
    strategy = strategy,
    parameters = parameters,
    cycles = cycles,
    init = init,
    method = method,
    inflow = inflow,
    strategy_name = strategy_name,
    expand_limit = expand_limit
  )
}
