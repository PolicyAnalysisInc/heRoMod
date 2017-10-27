#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#* Modified work Copyright (C) 2016  Matt Wiener
#* Modified work Copyright (C) 2017  Kevin Zarca
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
print.state <- function(x, ...) {
  cat(sprintf(
    "A state with %i value%s.\n\n",
    length(x), plur(length(x))))
  
  nv <- names(x)
  ex <- lapply(x, function(y) paste(deparse(y$expr), collapse = "\n"))
  
  cat(paste(nv, ex, sep = " = "), sep = "\n") 
}

#' @export
print.uneval_state_list <- function(x, ...) {
  n_state <- get_state_number(x)
  n_values <- length(get_state_value_names(x))
  
  cat(sprintf(
    "A list of %i state%s with %i value%s each.\n\n",
    n_state,
    plur(n_state),
    n_values,
    plur(n_values)
  ))
  cat("State names:\n\n")
  cat(get_state_names(x), sep = "\n")
  
  cat("\nState values:\n\n")
  cat(get_state_value_names(x), sep = "\n")
}

#' @export
print.eval_state_list <- function(x, ...) {
  cat(sprintf(
    "%i evaluated state%s, %i Markov cycle%s.\n",
    length(x),
    plur(length(x)),
    nrow(x[[1]]),
    plur(nrow(x[[1]]))
  ))
}
