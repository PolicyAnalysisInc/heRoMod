#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
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


#' @export
define_state_transition <- function(...) {
  if(is.na(from) && is.na(to)) stop("Either from or to must be defined.")
  .dots <- lazyeval::lazy_dots(...)
  
  define_state_transition_(.dots)
}

#' @export
define_state_transition_ <- function(.dots) {
  check_names(names(.dots))
  structure(
    .dots,
    class = c("state_transition", class(.dots))
  )
}
