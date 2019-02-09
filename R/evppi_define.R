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
#' @param par_names String of parameter name or names
#' 
#' @return An `evppi` object.
#' @export
#' 
#' @references 
#' \enumerate{
#' \item Jalal H, Alarid-Escudero F. A Gaussian Approximation Approach for Value of Information Analysis. Med Decis Making 2018; 38(2): 174-188. 
#' }
#' @example inst/examples/example_define_evppi.R
define_evppi <- function(...) {
  
  .dots <- lazyeval::lazy_dots(...)
  
  if (length(.dots) == 0) {
    stop("At least one parameter should be defined")
  }
  
  par_names <- character()
  
  for (i in seq_along(.dots)) {
    par_names <- c(par_names, deparse(.dots[[i]]$expr))
  }
  
  define_evppi_(par_names = par_names)
}

#' @rdname define_evppi
define_evppi_ <- function(par_names) {
  
  check_names(par_names)
  
  if (any(duplicated(par_names))) {
    stop("Some names are duplicated.")
  }
  
  structure(
    list(
      variable = par_names
    ),
    class = "evppi_definition"
  )
}

#' @export
print.evppi_definition <- function(x, ...) {
  print(paste0("An EVPPI definition for parameters: ",
               paste(x$variable, collapse = ", ")
  ))
}
