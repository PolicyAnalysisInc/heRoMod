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

#' Run Value-Based Price Analysis
#' 
#' @param model An evaluated Markov model.
#' @param vbp An object returned by 
#'   [define_vbp()].
#' @param strategy_vbp A string with strategy for vbp analysis.
#' @param wtp_thresholds A vector with WTP thresholds.
#' @return A `data.frame` with one row per model and 
#'   parameter value.
#' @export
#' 
#' @example inst/examples/example_run_vbp.R
run_vbp <- function(model, vbp, strategy_vbp, wtp_thresholds) {
  
  # Run some checks
  check_vbp(model, vbp, strategy_vbp)

  # Get names of intervention & comparators
  strategy_names <- get_strategy_names(model)
  strategy_comp <- strategy_names[strategy_names != strategy_vbp]
  
  # Calculate threshold values
  lambda <- seq(wtp_thresholds[1], wtp_thresholds[2], length.out = 100)
  
  n_par <- length(vbp$variable)
  pos_par <- cumsum(c(1, rep(c(n_par, n_par + 1), n_par)))
  pos_par <- c(pos_par[-length(pos_par)], 3)
  
  list_res <- list()
  e_newdata <- list()
  
  message(sprintf(
    "Running VBP on strategy '%s'...", strategy_vbp
  ))
  
  res <- purrr::map_dfr(strategy_names, function(n) {
    eval_strategy_newdata(
      model,
      strategy = n,
      newdata = vbp$vbp
    ) %>%
      mutate(
        .strategy_names = n,
        .par_names = vbp$variable,
        .par_value = purrr::map_dbl(.[[vbp$variable]], ~lazy_eval(.))
      ) %>%
      .[ ,colnames(.) != vbp$variable]
  })
    
res_vbp <- res %>%
  rowwise() %>%
  do(get_total_state_values(.$.mod)) %>%
  bind_cols(res %>% select(- .mod)) %>%
  ungroup() %>%
  mutate(.par_value_eval = unlist(e_newdata)) %>%
  mutate(!!!lazy_eval(get_ce(model), data = .)) %>%
  ungroup() %>%
  select(.strategy_names, .par_value, .cost, .effect)

  vbp_res_obj <- calc_vbp(res_vbp, lambda, strategy_vbp)
  
  df.p_vs_wtp <- vbp_res_obj$df.p_vs_wtp
  lin_eq <- vbp_res_obj$lin_eqs
  lin_approx <- vbp_res_obj$lin_approx
  m.p_vs_wtp <- as.matrix(df.p_vs_wtp)
  df.p_vs_wtp <- as.data.frame(m.p_vs_wtp)
  m.p_vs_wtp <- m.p_vs_wtp[, -1, drop = F]
  index.str.vbp <- max.col(-m.p_vs_wtp)
  
  df_p_vs_wtp.lg <- reshape2::melt(df.p_vs_wtp, 
                                   id.vars = "WTP", 
                                   value.name = "Price",
                                   variable.name = "Comparison")
  vbp_star <- matrixStats::rowMaxs(cbind(0,
                                         m.p_vs_wtp[cbind(1:length(lambda), 
                                                          index.str.vbp)]))
  df_vbp <- data.frame(WTP = lambda,
                       Price = vbp_star)
  
  structure(
    list(
      vbp        = df_vbp,
      lin_df = res_vbp,
      lambdas = lambda,
      vbp_strat = strategy_vbp,
      p_vs_wtp   = sort_by_strat(df_p_vs_wtp.lg, strategy_names, 'Comparison'),
      variable   = vbp$variable,
      lin_approx = lin_approx[match(strategy_names, names(lin_approx))],
      lin_eq = sort_by_strat(lin_eq, strategy_names, 'strat'),
      model      = model
    ),
    class = c("vbp", "list")
  )
}

calc_vbp <- function(df, lambdas, vbp_strat) {
  linear_cost <- df %>%
    group_by(.strategy_names) %>%
    group_split() %>%
    purrr::map(function(x) {
      linear <- c_linear(x, strategy = x$.strategy_names[1])
      tibble(
        .strategy_names = x$.strategy_names[1],
        .effect = x$.effect[1],
        beta0 = linear$beta0,
        beta1 = linear$beta1,
        lin_approx = linear$lin_approx
      )
    }) %>%
    bind_rows() %>%
    ungroup()
  
  ref <- filter(linear_cost, .strategy_names == vbp_strat)
  comps <- filter(linear_cost, .strategy_names != vbp_strat)
  
  lin_eqs <- comps %>%
    mutate(
      strat = .strategy_names,
      a = (ref$.effect - .effect) / (ref$beta1 - beta1),
      b = -(ref$beta0 - beta0) / (ref$beta1 - beta1)
    ) %>%
    select(strat, a, b)
  df.p_vs_wtp = comps %>%
    rowwise() %>%
    group_split() %>%
    purrr::map(function(comp) {
      tibble(
        .strategy_names = comp$.strategy_names,
        WTP = lambdas,
        value = p_comp(ref$.effect, comp$.effect, ref$beta0, comp$beta0, ref$beta1, comp$beta1, lambdas)
      )
    }) %>%
    bind_rows() %>%
    reshape2::dcast(WTP~.strategy_names, value = 'value')
  
  lin_approx <- as.list(linear_cost$lin_approx)
  names(lin_approx) <- linear_cost$.strategy_names
  
  list(
    lin_eqs = lin_eqs,
    lin_approx = lin_approx,
    df.p_vs_wtp = df.p_vs_wtp
  )
  
  
}

sort_by_strat <- function(df, strategy_names, colname) {
  df$.sort <- factor(df[[colname]], levels = strategy_names)
  sorted <- arrange(df, .sort) %>%
    select(-.sort)
  
  sorted
}

check_vbp <- function(model, vbp, strategy_vbp) {
  
  if (! all(c(".cost", ".effect") %in% names(get_model_results(model)))) {
    stop("No cost and effect defined, value-based pricing analysis unavailable.")
  }
  
  strategy_names <- get_strategy_names(model)
  
  if (! (strategy_vbp %in% strategy_names)) {
    stop("Strategy for VBP not defined")
  }
  
  if(!param_in_strategy(model, strategy_vbp, vbp$variable))
    stop(paste("Parameter", vbp$variable, "does not affect strategy", strategy_vbp))
  
}

get_model.vbp <- function(x) {
  x$model
}

#' Linearization of cost 
#' 
#' Calculates the linear equation of cost as a function of price per strategy
#' 
#' @param res_vbp The result of a VBP analysis
#' @param strategy Strategy
#' 
#' @return Intercept and slope of linearization, and indicator whether 
#' linearization was performed
#' 
c_linear <- function(res_vbp, strategy){
  message(sprintf(
    "Running linearization of cost on strategy '%s'...", strategy
  ))
  C_linear <- res_vbp %>% 
    filter(.strategy_names == strategy) %>%
    select(.par_value,
                  .cost) %>%
    mutate(price = as.numeric(.par_value)) %>%
    summarise(beta1 = diff(.cost[1:2])/diff(price[1:2]), # New
                     beta0 = .cost[1]-beta1*price[1])
  ### Linearization test
  lin_test <- res_vbp %>% 
    filter(.strategy_names == strategy) %>%
    select(.par_value,
                  .cost)
  p_vals <- as.numeric(lin_test$.par_value)
  c_test <- lin_test$.cost
  
  ### Predicted cost using linear function
  c_pred <- C_linear$beta0 + C_linear$beta1*p_vals
  
  ### Compute residual sum of squares (RSS)
  rss <- sum((c_pred - c_test)^2)
  
  ### Detemrine if relantionshipe is linear based on RSS
  if(isTRUE(all.equal(rss, 0))){
    message(sprintf(
      "--Relationship on strategy '%s' is linear", strategy
    ))
    ## No linear approximation is adopted
    lin_approx <- 0
  } else{
    message(sprintf(
      "--Relationship on strategy '%s' is NOT linear, using linear approximation", strategy
    ))
    ## Linear approximation is adopted
    lin_approx <- 1
  }
  
  return(list(beta0 = C_linear$beta0,
              beta1 = C_linear$beta1,
              lin_approx = lin_approx
  )
  )
}

#' Cost and effectiveness per strategy
#' 
#' Returns the cost and effectiveness for selected strategy
#' 
#' @param model A heRomod model
#' @param strategy Strategy
#' 
#' @return Cost and effectivness for selected strategy
#' 
ce_strategy <- function(model, strategy){
  CE_vbp <- get_model_results(model) %>% 
    filter(.strategy_names == strategy) %>%
    select(.strategy_names,
                  .cost,
                  .effect)
  return(list(
    e.strategy = CE_vbp$.effect, 
    c.strategy = CE_vbp$.cost
  )
  )
}

#' Price of comparator strategy
#' 
#' Returns the cost and effectiveness for selected strategy
#' 
#' @param e.P Effectiveness of strategy of interest
#' @param e.comp Effectiveness of comparator strategy
#' @param beta0.P Intercept of strategy of interest
#' @param beta0.comp Intercept of comparator strategy
#' @param beta1.P Slope of strategy of interest
#' @param beta1.comp Slope of comparator strategy
#' @param lambda Vector of willingness to pay thresholds
#' 
#' @return Vector of prices of comparator strategy
#' 
p_comp <- function(e.P, e.comp, beta0.P, beta0.comp, beta1.P, beta1.comp, lambda){
  p <- lambda*(e.P - e.comp)/(beta1.P - beta1.comp) - (beta0.P - beta0.comp)/(beta1.P - beta1.comp)
  return(p)
}

#' Parameter influences strategy
#' 
#' Determines if parameter influences strategy 
#' 
#' @param mod An evaluated Markov model
#' @param strategy Strategy of interest
#' @param parameter Parameter of interest
#' 
#' @return TRUE if parameter potentially influences strategy, FALSE otherwise
#' 
param_in_strategy <- function(mod,  strategy, parameter){
  # Obtain parameter list
  param_list <- mod$parameters
  # Obtian lazyeval of parameter of interest
  param_list[[parameter]] <- lazy(.._my_param)
  # Interpolate paramater list with lazyeval of parameter of interest
  i_params <- interpolate(param_list) # interpolate(params)
  
  ## States
  # Extract states
  state_list <- mod$uneval_strategy_list[[strategy]]$states
  i_state <- interpolate(state_list, more = as_expr_list(i_params)) 
  i_state <- i_state %>%
    unlist(recursive=F) %>%
    dispatch_strategy_substitute(strategy = strategy) %>%
    lapply(function(x) ".._my_param" %in% all.vars(x$expr)) %>%
    as.logical %>%
    any
  
  ## Transitions
  # Extract Transitions
  trans_list <- mod$uneval_strategy_list[[strategy]]$transition
  i_trans <- interpolate(trans_list, more = as_expr_list(i_params))
  if ("part_surv" %in% class(i_trans)) {
    i_trans <- i_trans[1:2]
  }
  i_trans <- i_trans %>%
    dispatch_strategy_substitute(strategy = strategy) %>%
    lapply(function(x) ".._my_param" %in% all.vars(x$expr)) %>%
    as.logical %>%
    any
  
  ## Starting values
  start_list <- mod$uneval_strategy_list[[strategy]]$starting_values
  i_start <- interpolate(start_list, more = as_expr_list(i_params)) 
  i_start <- i_start %>%
    dispatch_strategy_substitute(strategy = strategy) %>%
    lapply(function(x) ".._my_param" %in% all.vars(x$expr)) %>%
    as.logical %>%
    any
  
  # Return TRUE if parameter potentially influences strategy
  return(any(c(i_state, i_trans, i_start)))
}
