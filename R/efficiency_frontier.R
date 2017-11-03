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


#' Return Efficiency Frontier
#' 
#' @param x An `eval_strategy_list` object.
#'   
#' @return A vector of model names on the efficiency
#'   frontier.
#'   
#' @keywords internal
get_frontier <- function(x) {
  UseMethod("get_frontier")
}

get_frontier.default <- function(x) {
  # recursive function
  # if  all strat have same effect
  #     or root strat is more effective
  #   return less costly
  # else
  #   find root strat
  #   center on root strat
  #   remove less effective strat
  #   compute icer from root strat
  #
  #   return strats with NaN ICER
  #
  #   next strat on frontier: lowest icer & effect
  #   remove strat less effective than next strat
  #   recursively apply function on result
  
  if (stop_frontier(x)) {
    sort(
      (x %>% 
         dplyr::filter_(~ .cost == min(.cost)))$.strategy_names)
  } else {
    bm <- get_root_strategy(x)
    ebm <- x$.effect[x$.strategy_names == bm]
    cbm <- x$.cost[x$.strategy_names == bm]
    
    x$.effect <- x$.effect - ebm
    x$.cost <- x$.cost - cbm
    
    x <- x %>% 
      dplyr::filter_(~ .effect >= 0) %>% # not needed in theory
      dplyr::mutate_(
        .icer = ~ .cost / .effect
      ) %>% 
      dplyr::arrange_(~.icer, ~ .effect)
    
    enext <- dplyr::slice(x, 1)$.effect # relies on NaN last sorting
    
    x_res <- x %>% dplyr::filter_(
      substitute(.effect >= enext,
                 list(enext = enext)))
    # 0/0 = NaN = NA
    # x/0 = Inf != NA
    # is.na(.icer) excludes same effect more cost
    c(sort((dplyr::filter_(x, ~ is.na(.icer)))$.strategy_names),
      get_frontier(x_res))
  }
}

get_frontier.run_model <- function(x) {
  x$frontier
}

stop_frontier <- function(x) {
  length(unique(x$.effect)) == 1 ||
    get_root_strategy(x) %in% 
    (dplyr::filter_(x, ~ .effect == max(.effect)))$.strategy_names
}
