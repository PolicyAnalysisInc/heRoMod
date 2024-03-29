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


#' Plot Results of Probabilistic Analysis
#' 
#' Various plots for Markov models probabilistic analysis.
#' 
#' `type = "ac"` plots cost-effectiveness acceptability
#' curves, `type = "ce"` plots results on the 
#' cost-efficiency plane, `type = "cov"` to perform 
#' covariance analysis on the results, `type = "evpi"` 
#' for expected value of perfect information.
#' 
#' @param x Result from [run_model()].
#' @param type Type of plot, see details.
#' @param max_wtp Maximal willingness to pay.
#' @param n Number of CECA points to estimate (values above 
#'   100 may take significant time).
#' @param log_scale Show willingness to pay on a log scale?
#' @param diff Logical, perform covariance analysis on
#'   strategy differences?
#' @param threshold When `diff = TRUE`, threshlod value
#'   for net monetary benefit computation.
#' @param bw Black & white plot for publications?
#' @param ... Additional arguments, depends on `type`.
#'   
#' @return A `ggplot2` object.
#' @export
#' 
plot.psa <- function(x, type = c("ce", "ac", "cov", "evpi"),
                     max_wtp = 1e5,
                     n = 100, log_scale = TRUE,
                     diff = FALSE, threshold,
                     bw = FALSE, ...) {
  type <- match.arg(type)
  
  switch(
    type,
    ce = {
      tab <- scale(x)
      res <- ggplot2::ggplot(data = tab,
                             ggplot2::aes(
                               x = .effect,
                               y = .cost,
                               colour = .strategy_names)) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_hue(name = "Model") +
        ggplot2::xlab("Incremental effect") +
        ggplot2::ylab("Incremental cost")
      
      if (bw) {
        res <- res +
          ggplot2::scale_color_grey(start = 0, end = .8,
                                    name = "Strategy") +
          theme_pub_bw()
      }
      
      res
    },
    ac = {
      values <- generate_wtp(max_wtp = max_wtp,
                             n = n, log_scale = log_scale)
      tab <- acceptability_curve(x$psa, values)
      
      res <- ggplot2::ggplot(tab, 
                             ggplot2::aes(
                               x = .ceac,
                               y = .p,
                               colour = .strategy_names)) +
        ggplot2::geom_line() +
        ggplot2::ylim(0, 1) +
        ggplot2::scale_colour_hue(name = "Strategy") +
        ggplot2::xlab("Willingness to pay") +
        ggplot2::ylab("Probability of cost-effectiveness")
      
      if (log_scale) {
        res <- res +
          ggplot2::scale_x_log10()
      }
      
      if (bw) {
        res <- res +
          ggplot2::scale_color_grey(start = 0, end = .8,
                                    name = "Strategy") +
          theme_pub_bw()
      }
      
      res
    },
    evpi = {
      values <- generate_wtp(max_wtp = max_wtp,
                             n = n, log_scale = log_scale)
      tab <- compute_evpi(x, values)
      
      res <- ggplot2::ggplot(tab,
                             ggplot2::aes(
                               x = .ceac,
                               y = .evpi
                             )) +
        ggplot2::geom_line() +
        ggplot2::xlab("Willingness to pay") +
        ggplot2::ylab("EVPI") +
        ggplot2::ylim(0, NA)
      
      if (log_scale) {
        res <- res +
          ggplot2::scale_x_log10()
      }
      
      res
    },
    cov = {
      tab <- compute_cov(x, diff = diff, threshold = threshold, ...) %>% 
        mutate(
          .prop = .prop * 100
        )
      
      ggplot2::ggplot(
        tab,
        ggplot2::aes(.par_names, .prop)) +
        ggplot2::geom_col() +
        ggplot2::facet_grid(.result ~ .strategy_names) +
        ggplot2::xlab("Parameter") +
        ggplot2::ylab("Variance explained (%)") +
        ggplot2::coord_flip()
    },
    stop("Unknown plot type."))
}

#' @rdname heRomod_scale
scale.psa <- function(x, center = TRUE, scale = TRUE) {
  .bm <- get_central_strategy(x)
  
  res <- x$psa
  
  if (scale) {
    res <- res %>% 
      mutate(
        .cost = .cost / .n_indiv,
        .effect = .effect / .n_indiv
      )
  }
  
  if (center) {
    res <- res %>% 
      group_by(.index) %>% 
      mutate(
        .cost = (.cost - sum(.cost * (.strategy_names == .bm))),
        .effect = (.effect - sum(.effect * (.strategy_names == .bm)))
      ) %>% 
      ungroup()
  }
  
  res
}

#' @export
summary.psa <- function(object, threshold = NULL, ...) {
  summary.run_model(object = object, threshold = threshold, ...)
}

#' @export
print.psa <- function(x, ...) {
  cat(sprintf(
    "A PSA with %i resamplings.\n\n",
    x$N
  ))
  
  print(summary(x), ...)
}

get_frontier.psa <- function(x) {
  get_frontier(get_model_results(x))
}
