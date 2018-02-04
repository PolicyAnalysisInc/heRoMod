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


#' Define an Expected Value of Partial Perfect Information (EVPPI) Analysis
#' 
#' Define the parameter for EVPPI analysis of Markov models using a
#' linear regression metmodeling approach
#' 
#' The parameter name should be a string 
#' 
#' @param par_name String of parameter name
#' 
#' @return An `evppi` object.
#' @export
#' 
#' @references 
#' \enumerate{
#' \item Jalal H, Alarid-Escudero F. A Gaussian Approximation Approach for Value of Information Analysis. 
#' Med Decis Making 2018; 38: 174–188. 
#' }
#' @example inst/examples/example_define_evppi.R
#'   
define_evppi <- function(...) {

  .dots <- lazyeval::lazy_dots(...)
  
  if (! length(.dots) %% 1 == 0) {
    stop("At least one parameter should be considered")
  }
  
  par_name <- deparse(.dots[[1]]$expr)
  
  define_evppi_(par_name = par_name)
}

#' @rdname define_evppi
define_evppi_ <- function(par_name) {
  
  check_names(par_name)
  
  structure(
    list(
      variable = par_name
    ),
    class = "evppi_definition"
  )
}

#' @export
print.evppi_definition <- function(x, ...) {
  cat(sprintf(
    "An EVPPI definition for parameter %s",
    x$variable
  ))
}
