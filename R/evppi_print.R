#**************************************************************************
#* 
#* Original work Copyright (C) 2018  Fernando Alarid-Escudero
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


#' Plot Results of Expected Value of Partial Perfect Information Analysis
#' 
#' Plot of EVPPI for one or more parameters by willingness to pay threshold
#' 
#' @param x Result from [compute_evppi()].
#' @param bw Black & white plot for publications?
#' @param ... Additional arguments, depends on `type`.
#'   
#' @return A `ggplot2` object.
#' @export
#' 
plot.evppi_res <- function(x,
                           bw = FALSE, ...) {
  
  x.lng <- reshape2::melt(x$evppi_res, 
                          id.vars = "WTP", 
                          variable.name = "Parameter",
                          value.name = "EVPPI")
  
  if(length(unique(x.lng$WTP)) == 1){
    res <- ggplot2::ggplot(x.lng,
                           ggplot2::aes(
                             x = Parameter,
                             y = EVPPI,
                             color = Parameter,
                             fill = Parameter
                           )) +
      ggplot2::geom_col() +
      ggplot2::xlab("Parameter") +
      ggplot2::ylab("EVPPI") +
      ggplot2::ylim(0, NA) 
  } else {
    res <- ggplot2::ggplot(x.lng,
                           ggplot2::aes(
                             x = WTP,
                             y = EVPPI,
                             # linetype = Parameter,
                             color = Parameter
                           )) +
      ggplot2::geom_line() +
      ggplot2::ylim(0, NA) +
      ggplot2::scale_colour_hue(name = "Parameter") +
      ggplot2::xlab("Willingness to pay") +
      ggplot2::ylab("EVPPI")
  }
  
  if (bw) {
    res <- res +
      ggplot2::scale_color_grey(start = 0, end = .8,
                                name = "Parameter") +
      ggplot2::scale_fill_grey(start = 0, end = .8,
                                name = "Parameter") +
      theme_pub_bw()
  }
  res
}

#' @export
print.evppi_res <- function(x, ...) {
  v <- x$variables
  cat(sprintf("EVPPI on %i parameters over %i WTP threshold values.\n\n",
              length(v),
              length(x$evppi_res$WTP)))
  cat(paste(c("Parameters:", v), "\n", collapse = "\n  -"))
  
  print(x$evppi_res)
}
