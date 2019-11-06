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

#' Acceptability Curve from Probabilistic Analysis
#' 
#' @param x Result from [run_psa()].
#' @param wtp_thresholds willingness to pay thresholds
#'   
#' @return A data frame with columns `.ceac` (the 
#'   cost-effectiveness acceptability threshold), 
#'   `.model` (treatments or models), `.n` (the 
#'   number of cases in which the treatment was most 
#'   cost-effective), and `.p` (the proportion of cases
#'   where the treatment was most effective).
#'   
#' @keywords internal
#' @section Copyright: 2016  Antoine Pierucci
acceptability_curve <- function(x, wtp_thresholds) {
  x %>% 
    mutate(.key = 1) %>% 
    left_join(
      tibble::tibble(
        .ceac = wtp_thresholds,
        .key = 1
      ),
      by = ".key"
    ) %>% 
    group_by(.index, .ceac) %>% 
    mutate(
      .nmb = .effect * .ceac - .cost,
      .top_strategy = .nmb == max(.nmb),
      .top_strategy = .top_strategy & cumsum(.top_strategy) == 1
      # in case 2 nmb are identical, pick first
    ) %>% 
    group_by(.ceac, .strategy_names) %>% 
    summarise(.n = sum(.top_strategy)) %>% 
    group_by(.ceac) %>% 
    mutate(.p = .n / sum(.n))
}

generate_wtp <- function(max_wtp,
                         min_wtp = max_wtp / 1000,
                         n, log_scale) {
  stopifnot(
    max_wtp > 0
  )
  if (log_scale) {
    res <- seq(from = log(min_wtp, base = 10),
               to = log(max_wtp, base = 10),
               length.out = n)
    10 ^ res
  } else {
    seq(from = 0, to = max_wtp, length.out = n)
  }
}
