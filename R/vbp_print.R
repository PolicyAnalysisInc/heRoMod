#**************************************************************************
#* 
#* Original work Copyright (C) 2018  Fernando Alarid-Escudero
#* Original work Copyright (C) 2018  Jordan Amdahl
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

#' Plot Sensitivity Analysis
#' 
#' Plot the results of a sensitivity analysis as a tornado 
#' plot.
#' 
#' Plot type `simple` plots variations of single strategy 
#' values, while `difference` plots incremental values.
#' 
#' @param x A result of [run_vbp()].
#' @param bw Black & white plot for publications?
#' @param ... Additional arguments passed to `plot`.
#'   
#' @return A `ggplot2` object.
#' @export
#' 
plot.vbp <- function(x, 
                     bw = FALSE, ...) {
  res <- ggplot2::ggplot(x$p_vs_wtp, 
                         aes(x = WTP, y = Price, color = Comparison)) +
    ggplot2::geom_line(data = x$vbp, 
                       aes(x = WTP, y = Price), 
      linetype = 1,
      color = "black") +
    ggplot2::geom_line(linetype = "dotted") +
    ggplot2::xlab("WTP Threshold")
  
  if (bw) {
    res <- res +
      ggplot2::scale_color_grey(start = 0.3, end = .8) +
      theme_pub_bw()
  }
  
  res
}

#' @export
print.summary_vbp <- function(x, ...) {
  cat(sprintf(
    "VBP of parameter %s.\n\n",
    x$res_strategy
  ))
  
  print_results_vbp(x$res_values)
}

print_results_vbp <- function(res_values) {
  print(res_values)
}

#' @export
print.vbp <- function(x, ...) {
  print(summary(x))
}

get_central_strategy.vbp <- function(x, ...) {
  get_central_strategy(get_model(x))
}

#' Summarise Value-Based Pricing Results
#' 
#' @param x Output from [run_vbp()].
#' @param ... additional arguments affecting the summary 
#'   produced.
#'   
#' @return A `summary_vbp` object.
#' @export
summary.vbp <- function(x, ...) {
  
  res_values <- x$vbp %>% 
    dplyr::filter(row_number()==1 | row_number()==n()) %>% 
    dplyr::rename(VBP = Price) %>%
    as.data.frame()
  
  
  structure(
    list(
      res_values   = res_values,
      res_strategy = x$variable
    ),
    class = "summary_vbp"
  )
}
