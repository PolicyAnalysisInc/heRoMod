#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#* Modified work Copyright (C) 2017  Matt Wiener
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


#' Run Model on New Data
#' 
#' Given a table of new parameter values with a new 
#' parameter set per line, runs iteratively Markov models 
#' over these sets.
#' 
#' `newdata` must be a `data.frame` with the
#' following properties: the column names must be parameter 
#' names used in [define_parameters()]; and an
#' optional column `.weights` can give the respective
#' weight of each row in the target population.
#' 
#' Weights are automatillcally scaled. If no weights are
#' provided equal weights are used for each strata.
#' 
#' For the plotting function, the `type` argument can
#' take the following values: `"cost"`, `"effect"`
#' or `"icer"` to plot the heterogeneity of the
#' respective values. Furthermore `"ce"` and
#' `"count"` can produce from the combined model plots
#' similar to those of [run_model()].
#' 
#' @name update_model
#' @param object The result of [run_model()].
#' @param newdata A `data.frame` of new parameter sets,
#'   one column per parameter and one row per parameter set.
#'   An optional `.weights` column can be included for
#'   a weighted analysis.
#' @param x Updated model to plot.
#' @param strategy A model index, character or numeric.
#' @param result The the result to plot (see details).
#' @param type Plot simple values or differences?
#' @param ... Additional arguments passed to
#'   `geom_histogram`. Especially usefull to specify
#'   `binwidth`.
#'   
#' @section Warning:
#'   
#'   Histograms do not account for weights. On the other
#'   hand summary results do.
#'   
#' @return A `data.frame` with one row per model/value.
#' @export
#' 
#' @example inst/examples/example_update.R
#'   
update.run_model <- function(object, newdata, ...) {
  
  if (!any(class(object) %in% "run_model")) {
    stop("'object' must be the result of 'run_model()'.")
  }
  
  has_weights <- ".weights" %in% names(newdata)
  
  if (has_weights) {
    weights <- newdata$.weights
    newdata <- select(newdata, -.weights)
    
  } else {
    message("No weights specified in update, using equal weights.")
    weights <- rep(1, nrow(newdata))
  }
  
  ce <- get_ce(object)
  list_res <- list()
  
  for (n in get_strategy_names(object)) {
    message(sprintf("Updating strategy '%s'...", n))
    suppressMessages({
      list_res <- c(
        list_res,
        list(eval_strategy_newdata(
          object, strategy = n, 
          newdata = newdata,
          ...
        ))
      )
    })
  }
  
  names(list_res) <- get_strategy_names(object)
  
  for (n in names(list_res)) {
    list_res[[n]]$.strategy_names <- n
    list_res[[n]]$.index <- seq_len(nrow(newdata))
  }
  
  res <- 
    bind_rows(list_res)
  suppressMessages({
    res_total <- res %>% 
      rowwise() %>% 
      do(get_total_state_values(.$.mod)) %>% 
      bind_cols(res %>% select(-.mod)) %>% 
      ungroup() %>% 
      mutate(!!!lazy_eval(ce, data = .)) %>% 
      left_join(
        tibble(
          .index = seq_len(nrow(newdata)),
          .weights = weights
        )
      ) %>% 
      ungroup()
  })
  
  comb_mods <- combine_models(
    newmodels = list_res,
    weights = weights,
    oldmodel = object
  )
  
  structure(
    list(
      updated_model = res_total,
      model_list = list_res,
      newdata = newdata,
      model = object,
      combined_model = comb_mods,
      has_weights = has_weights,
      weights = weights
    ),
    class = c("updated_model", class(res))
  )
}

#' @export
print.updated_model <- function(x, ...) {
  print(summary(x), ...)
}

#' @export
#' @rdname update_model
plot.updated_model <- function(x, type = c("simple", "difference",
                                           "counts", "ce", "values"),
                               result = c("cost", "effect", "icer"),
                               strategy = NULL,
                               ...) {
  type <- match.arg(type)
  result <- match.arg(result)
  
  if (type %in% c("counts", "ce", "values")) {
    return(plot(x$combined_model,
                type = type, strategy = strategy,
                ...) 
    )
  }
  if (is.null(strategy)) {
    strategy <- get_strategy_names(get_model(x))
    
    if (type == "difference") {
      strategy <- setdiff(strategy, get_noncomparable_strategy(get_model(x)))
    }
    
  } else {
    strategy <- check_strategy_index(
      get_model(x),
      strategy,
      allow_multiple = TRUE
    )
  }
  
  if (get_noncomparable_strategy(get_model(x)) %in% strategy &&
      "difference" %in% type) {
    stop("Cannot represent value differences from uncomparable strategy.")
  }
  
  if (type == "simple" && result == "icer") {
    stop("Result 'icer' can conly be computed with type = 'difference'.")
  }
  
  switch(
    paste(type, result, sep = "_"),
    simple_cost = {
      x_var <- ".cost"
      x_lab <- "Cost"
    },
    simple_effect = {
      x_var <- ".effect"
      x_lab <- "Effect"
    },
    difference_cost = {
      x_var <- ".dcost"
      x_lab <- "Cost Diff."
    },
    difference_effect = {
      x_var <- ".deffect"
      x_lab <- "Effect Diff."
    },
    difference_icer = {
      x_var <- ".icer"
      x_lab <- "ICER"
    }
  )
  summary(x)$scaled_results %>% 
    filter(.strategy_names %in% strategy) %>% 
    ggplot2::ggplot(ggplot2::aes(x = !!sym(x_var))) +
    ggplot2::geom_histogram(...) +
    ggplot2::xlab(x_lab)+
    ggplot2::facet_grid(.strategy_names ~ .)
}

scale.updated_model <- function(x, scale = TRUE, center = TRUE) {
  .bm <- get_root_strategy(get_model(x))
  
  res <- x$updated_model
  
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

get_model.updated_model <- function(x) {
  x$model
}

#' @export
summary.updated_model <- function(object, ...) {
  
  strategy_names <- get_strategy_names(
    get_model(object)
  )[ord_eff <- order(get_effect(get_model(object)))]
  
  list_res <- list()
  
  tab_scaled <- object %>% 
    scale(center = FALSE) %>% 
    group_by(.index) %>% 
    do(compute_icer(
      ., strategy_order = ord_eff)
    )
  
  for (.n in strategy_names) {
    
    tmp <- tab_scaled %>%
      filter(.strategy_names == .n)
    
    list_res <- c(
      list_res,
      lapply(
        c(".cost", ".effect", ".dcost", ".deffect", ".icer"),
        function(x) {
          wsum <- wtd_summary(
            tmp[[x]],
            tmp$.weights
          )
          is.na(wsum) <- ! is.finite(wsum)
          tab_summary <- matrix(
            wsum,
            nrow = 1
          )
          colnames(tab_summary) <- names(wsum)
          cbind(
            data.frame(Model = .n,
                       Value = x),
            tab_summary
          )
        }
      )
    )
  }
  
  tab_res <- 
    do.call("rbind", list_res)
  tab_res$Value <- tab_res$Value %>% 
    factor(
      levels = c(".cost", ".effect",
                 ".dcost", ".deffect", 
                 ".icer"),
      labels = c("Cost", "Effect",
                 "Cost Diff.", "Effect Diff.",
                 "Icer")
    )
  
  mat_res <- select(
    tab_res,
    - Model,
    - Value
  ) %>% 
    as.matrix()
  
  rownames(mat_res) <- tab_res$Model %>% 
    paste(tab_res$Value, sep = " - ")
  
  structure(
    list(
      summary_results = tab_res,
      scaled_results = tab_scaled,
      model = object,
      to_print = mat_res,
      sum_comb = summary(object$combined_model, ...)
    ),
    class = c("summary_updated_model", class(tab_res))
  )
}

get_model.summary_updated_model <- function(x) {
  x$model
}

#' @export
print.summary_updated_model <- function(x, ...) {
  object <- get_model(x)
  
  cat(sprintf(
    "An analysis re-run on %i parameter sets.\n\n",
    nrow(object$newdata)
  ))
  
  if (! object$has_weights) {
    cat("* Unweighted analysis.")
  } else {
    cat("* Weigths distribution:\n\n")
    print(summary(object$weights))
    cat(sprintf("\nTotal weight: %s",
                format(sum(object$weights))))
  }
  
  cat("\n\n* Values distribution:\n\n")
  
  print(x$to_print, na.print = "-")
  
  cat("\n* Combined result:\n\n")
  
  print(x$sum_comb)
  
  invisible(x)
}

get_newdata <- function(x) {
  x$newdata
}
