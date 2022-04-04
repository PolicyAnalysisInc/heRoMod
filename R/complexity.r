get_bc_max_progress <- function(model) {
  n_strat <- get_n_strats(model)
  n_group <- get_n_groups(model)
  
  2 + 10 * (n_strat + (n_group > 1) * n_group * n_strat)
}

get_vbp_max_progress <- function(model) {
  n_strat <- get_n_strats(model)
  n_group <- get_n_groups(model)
  
  2 + 10 * (n_strat + 3 * n_strat * n_group)
}

get_dsa_max_progress <- function(model) {
  n_strat <- get_n_strats(model)
  n_group <- get_n_groups(model)
  n_param <- get_n_dsa_params(model)
  run_vbp <- get_dsa_run_vbp(model)
  
 2 + 10 * (n_strat + (n_param * 2 + 1) * n_group * n_strat * ifelse(run_vbp, 4, 1))
}

get_twsa_max_progress <- function(model, sa_table) {
  n_strat <- get_n_strats(model)
  n_group <- get_n_groups(model)
  
  2 + 10 * (n_strat + n_strat * nrow(sa_table))
}

get_scen_max_progress <- function(model) {
  n_strat <- get_n_strats(model)
  n_group <- get_n_groups(model)
  n_scen <- get_n_scen(model)
  run_vbp <- get_scen_run_vpb(model)
  
  2 + 10 * (n_strat + (n_scen + 1) * n_group * n_strat * ifelse(run_vbp, 4, 1))
}

get_psa_max_progress <- function(model) {
  n_strat <- get_n_strats(model)
  n_group <- get_n_groups(model)
  n_iter <- get_n_psa_iter(model)
  
  2 + 10 * (n_strat + n_strat * n_group * n_iter)
}

get_excel_max_progress <- function(model) {
  get_bc_max_progress(model)
}

get_code_preview_max_progress <- function(model) {
  return(5)
}

get_r_project_max_progress <- function(model) {
  return(5)
}


get_threshold_max_progress <- function(model) {
  return(10 + get_n_threshold_analyses(model) * 100)
}

get_n_threshold_analyses <- function(model) {
  max(nrow(model$threshold_analyses), 1)
}

get_n_strats <- function(model) {
  nrow(model$strategies)
}

get_n_groups <- function(model) {
  max(nrow(model$groups), 1)
}

get_n_dsa_params <- function(model) {
  sum(!(is.na(model$variables$low) | model$variables$low == ''))
}

get_dsa_run_vbp <- function(model) {
  !is.null(model$dsa_settings) &&
    !is.null(model$dsa_settings$run_vbp) &&
    model$dsa_settings$run_vbp
}

get_scen_run_vpb <- function(model) {
  !is.null(model$scenario_settings) &&
    !is.null(model$scenario_settings$run_vbp) &&
    model$scenario_settings$run_vbp
}

get_n_scen <- function(model) {
  length(unique(model$scenario$scenario_name))
}

get_n_psa_iter <- function(model) {
  model$psa$n
}