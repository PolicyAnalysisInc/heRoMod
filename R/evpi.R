#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
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


compute_evpi <- function(x, wtp_thresholds) {
  x$psa %>% 
    mutate(.key = 1) %>% 
    left_join(
      tibble::tibble(
        .ceac = wtp_thresholds,
        .key = 1,
        .strategy_choice = summary(
          x, threshold = wtp_thresholds)$res_nmb_strat
      ),
      by = ".key"
    ) %>% 
    group_by(.ceac, .index) %>% 
    mutate(
      .nmb = .effect * .ceac - .cost,
      .top_strategy = .nmb == max(.nmb),
      .top_strategy = .top_strategy & cumsum(.top_strategy) == 1,
      .top_choice = .strategy_names == .strategy_choice
      # in case 2 nmb are identical, pick first
    ) %>% 
    summarise(
      .evpi = .nmb[.top_strategy] - .nmb[.top_choice]
    ) %>% 
    summarise(
      .evpi = mean(.evpi)
    )
}

#' Export PSA Results for SAVI
#' 
#' Export the result of a PSA in a format compatible with 
#' Sheffield Accelerated Value of Information software.
#' 
#' This function saves 3 files at the path given by
#' `folder`: `param.csv`, the parameter values,
#' `cost.csv` and `effect.csv` the cost and effect
#' results.
#' 
#' The official SAVI website can be found at this URL: 
#' http://http://savi.shef.ac.uk/SAVI/
#' 
#' @param x PSA result.
#' @param folder A folder where to save the `csv` files.
#'   
#' @return Nothing. Creates 3 files.
#' @export
export_savi <- function(x, folder = ".") {
  res <- export_psa(x)
  
  write.csv(
    x = res$par,
    file = file.path(folder, "param.csv"),
    row.names = FALSE)
  
  write.csv(
    x = res$c,
    file = file.path(folder, "cost.csv"),
    row.names = FALSE
  )
  
  write.csv(
    x = res$e,
    file = file.path(folder, "effect.csv"),
    row.names = FALSE
  )
}

export_psa <- function(x) {
  strats <- unique(x$psa$.strategy_names)
  
  list(
    par = x$psa[x$psa$.strategy_names == strats[1],x$resamp_par],
    c = x$psa %>%
      select(.strategy_names, .cost, .index) %>%
      reshape2::dcast(.index~.strategy_names, value.var = ".cost") %>%
      select(-.index),
    e = x$psa %>%
      select(.strategy_names, .effect, .index) %>%
      reshape2::dcast(.index~.strategy_names, value.var = ".effect") %>%
      select(-.index)
  )
}

#' Use the BCEA package
#' 
#' Interfaces the output of [run_psa()] into the BCEA package.
#' 
#' The BCEA package is needed for this function to work.
#' 
#' @param x Output from [run_psa()].
#' @param ... Additional arguemnts passed to [BCEA::bcea()].
#'   
#' @return A BCEA analysis.
#' @export
#' 
run_bcea <- function(x, ...) {
  if (! requireNamespace("BCEA")) {
    stop("'BCEA' package required for BCEA analysis.")
  }
  
  res <- export_psa(x)
  
  BCEA::bcea(
    e = as.matrix(res$e),
    c = as.matrix(res$c),
    interventions = get_strategy_names(x),
    ...
  )
}
