#' @export
run_hero_vbp <- function(...) {
  
  # Build model object
  dots <- patch_progress_funcs(list(...))
  args <- do.call(build_hero_model, dots)
  
  max_prog <- get_vbp_max_progress(dots)
  try(dots$report_max_progress(max_prog))
  
  # Initial model run
  heemod_res <- do.call(run_model_api, args)
  vbp_name <- dots$vbp$par_name
  
  # Generate sensitvity analysis input table
  groups_table <- gen_groups_table(dots$groups)
  check_hero_vbp(dots$vbp)
  dots$vbp$vbp_only <- T
  vbp_table <- gen_vbp_table(dots$vbp)
  sa_table <- crossing(groups_table, vbp_table)
  n_row <- nrow(sa_table)
  indices <- rep(T, n_row)
  if (vbp_name %in% colnames(sa_table)) {
    indices <- !is.na(sa_table$.vbp_param)
  }
  sa_table[[vbp_name]][indices] <- sa_table$.vbp_param[indices]
  sa_table <- select(sa_table, -.vbp_param) %>%
    dplyr::relocate(.group_scen, .group_weight, .vbp_scen, .vbp_price)
  
  # Run sensitivity Analyses
  res <- run_sa(
    heemod_res$model_runs,
    sa_table, c(),
    report_progress = dots$report_progress,
    heemod_res$model_runs$cores
  )
  
  # Pull out results for each scenario
  outcomes_res <- extract_sa_summary_res(res, dots$hsumms, c())
  costs_res <- extract_sa_summary_res(res, dots$esumms, c())
  vbp_res <- extract_sa_vbp(outcomes_res, costs_res, dots$vbp, dots$hsumms, c())
  
  # Format and Return
  list(
    eq = vbp_format_lin_eq(vbp_res, dots$strategies),
    vbp = vbp_format_vbp(vbp_res, dots$strategies),
    referent = dots$vbp$strat
  )
}



check_hero_vbp <- function(vbp) {
  
  if (is.null(vbp)) {
    stop(error_codes$vbp_settings_undefined, call. = F)
  }
  
  if (!('list' %in% class(vbp))) {
    stop(error_codes$vbp_settings_undefined, call. = F)
  }
  
  if (is.null(vbp$strat) || is.na(vbp$strat) || vbp$strat == '') {
    stop(error_codes$vbp_settings_undefined, call. = F)
  }
  
  if (is.null(vbp$par_name) || is.na(vbp$par_name) || vbp$par_name == '') {
    stop(error_codes$vbp_settings_undefined, call. = F)
  }
  
  if (is.null(vbp$cost) || is.na(vbp$cost) || vbp$cost == '') {
    stop(error_codes$vbp_settings_undefined, call. = F)
  }
  
  if (is.null(vbp$effect) || is.na(vbp$effect) || vbp$effect == '') {
    stop(error_codes$vbp_settings_undefined, call. = F)
  }
  
}

vbp_format_lin_eq <- function(res, strategies) {
  res %>%
    mutate(strat = series, a = slope, b = intercept) %>%
    select(strat, a, b) %>%
    arrange(factor(strat, levels = strategies$name))
}

vbp_format_vbp <- function(res, strategies) {
  res %>%
    mutate(strat = series) %>%
    select(strat, series, value) %>%
    arrange(factor(strat, levels = strategies$name))
}


# Generate a table containing parameter values to use in each simulation
# of VBP analysis.
gen_vbp_table <- function(vbp = NULL) {
  vbp_prices <- c(0, 1, 2)
  vbp_only <- F
  if (!is.null(vbp)) {
    if (!is.null(vbp$prices)) {
      vbp_prices <- vbp$prices
    }
    if (!is.null(vbp$vbp_only)) {
      vbp_only <- vbp$vbp_only
    }
  }
  
  vbp_scen_names <- c('low', 'middle', 'high')
  vbp_table <- create_sa_table(4, 1, '.vbp_param')
  vbp_table$.vbp_param <- c(list(NA), vbp_prices)
  vbp_table$.vbp_scen <- c(NA, vbp_scen_names)
  vbp_table$.vbp_price <- c(NA, vbp_prices)
  vbp_table <- dplyr::relocate(vbp_table, .vbp_scen, .vbp_price) %>%
    filter(!vbp_only | !is.na(.vbp_param))
  return(vbp_table)
}

# Given outcomes results form a model run at three different prices, check
# if outcomes vary by price.
check_price_doesnt_affect_outcomes <- function(outcomes) {
  # Extract outcomes
  values <- outcomes$value
  
  # Check if all results equal to first
  outcomes_not_affected <- all.equal(rep(values[1], 3), values)
  
  # If not, then throw error
  if (!(outcomes_not_affected) == TRUE) {
    stop(error_codes$vbp_affects_outcomes, call. = F)
  }
}

# Given the price, difference in cost, and difference in outcomes from
# running a model at three different prices, calculate the value-based
# equation.
#
# This VBP module works on the assumption that differences in costs are
# a linear function of the price parameter and that differences in outcome
# are unaffected by the price parameter. Based on these assumptions, the
# value-based price is a linear function of the WTP. This function returns
# the slope and intercept of that linear function.
calculate_vbp_equation <- function(price, delta_cost, delta_outcome) {
  
  # Check validity of inputs
  check_calculate_vbp_inputs(price, delta_cost, delta_outcome)
  
  # Check that outcomes aren't affected by results. Note that all.equal returns
  # a string if they aren't equal so can't directly use result as the condition.
  outcomes_not_affected <- all.equal(rep(delta_outcome[1], 3), delta_outcome)
  if (!(outcomes_not_affected == TRUE)) {
    stop(error_codes$vbp_affects_outcomes, call. = F)
  }
  
  # Generate linear models of cost as a function of price
  linear_model <- get_linear_model_and_check(price, delta_cost)
  
  # Throw error if result isn't perfectly linear
  if (!linear_model$linear) {
    stop(error_codes$vbp_not_linear, call. = F)
  }
  
  # Calculate and return the slope and intercept of VBP as a function of WTP
  vbp_slope = delta_outcome[1] / linear_model$slope
  vbp_intercept = -linear_model$intercept / linear_model$slope
  return(list(
    slope = vbp_slope,
    intercept = vbp_intercept,
    cost_slope = linear_model$slope,
    cost_intercept = linear_model$intercept
  ))
}

# Given the wtp and the slope/intercept of the VBP equation, calculate the VBP
calculate_vbp <- function(wtp, intercept, slope) {
  return(wtp * slope + intercept)
}

# Generate linear model and check whether it is perfectly linear
get_linear_model_and_check <- function(x, y) {
  # Run linear model
  linear_model <- lm(y ~ x)
  coef <- unname(linear_model$coefficient)
  
  # Check if perfectly linear. Note that all.equal returns TRUE if the condition
  # is true but a string if the condition is false.
  linear_check <- all.equal(unname(linear_model$residuals), c(0,0,0), tolerance = 1E-5)
  
  # Return whether linear and slope/intercept
  return(list(linear = linear_check == T, intercept = coef[1], slope = coef[2]))
}

# Validate the inputs to calculate_vbp_input
check_calculate_vbp_inputs <- function(price, delta_cost, delta_outcome) {
  # Check that inputs are of right length and that there are no missing values
  stopifnot(
    length(price) == 3,
    length(delta_cost) == 3,
    length(delta_outcome) == 3,
    all(!is.na(price)),
    all(!is.na(delta_cost)),
    all(!is.na(delta_outcome))
  )
}
