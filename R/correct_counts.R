#**************************************************************************
#* 
#* Original work Copyright (C) 2017  Antoine Pierucci
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


correct_counts <- function(x, method = c("life-table",
                                         "beginning",
                                         "end")) {
  
  if (! is.function(method)) {
    method <- match.arg(method)
    
    n0 <- x[- nrow(x), ]
    n1 <- x[-1, ]
    
    switch(
      method,
      "beginning" = {
        out <- n1
      },
      "end" = {
        out <- n0
      },
      "life-table" = {
        out <- (n0 + n1) / 2
      })
  } else {
    out <- method(x)
  }
  
  if (nrow(out) != nrow(x) - 1) {
    stop("State membership correction applied to an n-row table should return a table with n-1 rows.")
  }
  
  if(!is.null(attr(x, "entry"))) {
    attr(out, "entry") <- attr(x, "entry")
  }
  
  if(!is.null(attr(x, "exit"))) {
    attr(out, "exit") <- attr(x, "exit")
  }
  
  if(!is.null(attr(x, "transitions"))) {
    attr(out, "transitions") <- attr(x, "transitions")
  }
  
  class(out) <- class(x)
  
  return(out)
}
