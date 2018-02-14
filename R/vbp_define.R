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

#' Define a Value-Based Pricing Analysis
#' 
#' Define parameter variations for a value-based 
#' pricing analysis.
#' 
#' @param ... A parameter name and min/max values 
#'   of the form \code{price, min(price), max(price)}.
#' @param par_name String vector of price parameter name.
#' @param low_dots,med_dots,high_dots Used to work around
#'   non-standard evaluation.
#'   
#' @return A `vbp` object.
#' @export
#' 
#' @examples
#' 
#' define_vbp(
#'   p, 0, 1000
#' )
#' 
define_vbp <- function(...) {
  .dots <- lazyeval::lazy_dots(...)
  
  if (! length(.dots) == 3) {
    stop("Incorrect number of elements in vbp definition, the correct form is price, min(price), max(price)")
  }
  
  par_name  <- character()
  low_dots  <- lazyeval::lazy_dots()
  med_dots  <- lazyeval::lazy_dots()
  high_dots <- lazyeval::lazy_dots()
  
  for (i in seq_along(.dots)) { # i <- 3
    if (i == 1) {
      par_name <- c(par_name, deparse(.dots[[i]]$expr))
    } else if (i == 2) {
      low_dots <- c(low_dots, list(.dots[[i]]))
    } else {
      high_dots <- c(high_dots, list(.dots[[i]]))
    }
  }
  
  # Compute mid-value between low and high price values
  med_dots <- high_dots
  med_dots[[1]]$expr <- c((lazyeval::lazy_eval(low_dots[[1]]) + lazyeval::lazy_eval(high_dots[[1]]))/2)
  
  names(low_dots)  <- par_name
  names(med_dots)  <- par_name
  names(high_dots) <- par_name
  
  define_vbp_(par_name  = par_name,
              low_dots  = low_dots,
              med_dots  = med_dots,
              high_dots = high_dots)
}

#' @rdname define_vbp
define_vbp_ <- function(par_name, low_dots, med_dots, high_dots) {
  
  check_names(par_name)
  
  stopifnot(
    all(par_name == names(low_dots)),
    all(par_name == names(med_dots)),
    all(par_name == names(high_dots))
  )
  dots <- interleave(low_dots, med_dots, high_dots)
  
  # if (any(duplicated(par_name))) {
  #   stop("Some names are duplicated.")
  # }
  
  tab <- tibble::tibble()
  for (i in seq_along(dots)) {
    suppressWarnings({ # tofix https://github.com/tidyverse/dplyr/issues/2688
      tab <- dplyr::bind_rows(
        tab,
        stats::setNames(tibble::tibble(dots[i]), names(dots)[i])
      )
    })
  }
  
  clean_null <- function(x) {
    Map(
      function(el) if (is.null(el)) NA else el,
      x
    )
  }
  
  structure(
    list(
      vbp = tab %>% 
        dplyr::mutate_all(dplyr::funs(clean_null)),
      variable  = par_name,
      low_dots  = low_dots,
      med_dots  = med_dots,
      high_dots = high_dots
    ),
    class = c("vbp_def", class(tab))
  )
}

#' @export
print.vbp_def <- function(x, ...) {
  tab <- cbind(to_text_dots(x$low_dots, name = FALSE),
               to_text_dots(x$high_dots, name = FALSE))
  
  rownames(tab) <- x$variable
  colnames(tab) <- c("Low", "High")
  
  print(
    tab,
    na.print = "-",
    quote = FALSE
  )
}
