#' @export
run_hero_scen <- function(...) {
  
  # Build model object
  dots <- list(...)
  check_scenarios(dots$scenario)
  args <- do.call(build_hero_model, dots)
  
  # Initial model run
  heemod_res <- do.call(run_model_api, args)
  vbp_name <- dots$vbp$par_name
  
  # Generate sensitvity analysis input table
  scen_table <- gen_scenario_table(dots$scenario)
  groups_table <- gen_groups_table(dots$groups)
  if (is.null(dots$scenario_settings) || !dots$scenario_settings$run_vbp) {
    vbp_table <- tibble(.vbp_scen = NA, .vbp_price = NA, .vbp_param = list(NA))
    run_vbp <- FALSE
  } else {
    vbp_table <- gen_vbp_table(dots$vbp)
    run_vbp <- TRUE
  }
  sa_table <- crossing(groups_table, scen_table, vbp_table)
  n_row <- nrow(sa_table)
  indices <- rep(T, n_row)
  if (vbp_name %in% colnames(sa_table)) {
    indices <- !is.na(sa_table$.vbp_param)
  }
  sa_table[[vbp_name]][indices] <- sa_table$.vbp_param[indices]
  sa_table <- select(sa_table, -.vbp_param) %>%
    dplyr::relocate(.scenario, .group_scen, .group_weight, .vbp_scen, .vbp_price)
  
  # Run sensitivity Analyses
  res <- run_sa(heemod_res$model_runs, sa_table, c('.scenario'), report_progress = dots$report_progress)
  
  # Pull out results for each scenario
  outcomes_res <- extract_sa_summary_res(res, dots$hsumms, c('.scenario'))
  costs_res <- extract_sa_summary_res(res, dots$esumms, c('.scenario'))
  nmb_res <- extract_sa_nmb(outcomes_res, costs_res, dots$hsumms, dots$esumms, c('.scenario'))
  if (run_vbp) {
    vbp_res <- extract_sa_vbp(outcomes_res, costs_res, dots$vbp, dots$hsumms, c('.scenario'))
  }
  
  # Format and Return
  list(
    outcomes = scenario_format_res(outcomes_res, dots$scenario),
    cost = scenario_format_res(costs_res, dots$scenario),
    nmb = scenario_format_res(nmb_res, dots$scenario, id_vars = c('health_outcome', 'econ_outcome', 'series')),
    vbp = if (run_vbp) list(
        prices = scenario_format_res(vbp_res, dots$scenario, id_vars = c('series')),
        referent = dots$vbp$strat
      ) else NULL,
    api_ver = '2.0'
  )
  
}

gen_scenario_table <- function(scenarios) {
  
  # Extract scenario names and count number of scenarios
  scen_names <- unique(scenarios$scenario_name)
  n_scens <- length(scen_names)
  
  # Extract parameter names and count number of parameters
  param_names <- unique(scenarios$param_name)
  n_params <- length(param_names)
  
  # Create a table to store the parameter values to use in each iteration
  scen_table <- create_sa_table(n_scens + 1, n_params, param_names)
  
  # Populate the table with low/high parameter values
  for (scen_name in scen_names) {
    scen_params <- filter(scenarios, scenario_name == scen_name)
    n_scen_params <- nrow(scen_params)
    scen_index <- 1 + which(scen_name == scen_names)[1]
    for (i in seq_len(n_scen_params)) {
      param_name <- scen_params$param_name[i]
      param_formula <- scen_params$formula[i]
      scen_table[[param_name]][[scen_index]] <- create_sa_lazy_param(
        param_formula,
        context = glue('formula for parameter "{param_name}" in scenario "{scen_name}"', param_name = param_name, scen_name = scen_name)
      )
    }
  }
  
  scen_table <- mutate(
    scen_table,
    .scenario = c(NA, scen_names)
  ) %>%
    dplyr::relocate(.scenario)
  
  return(scen_table)
}

# Check the validity of scenarios object
check_scenarios <- function(scenarios) {
  
  # Check that it isn't NULL
  if (is.null(scenarios)) {
    stop(error_codes$scenario_null, call. = F)
  }
  
  # Check that it is a dataframe
  if (!('data.frame' %in% class(scenarios))) {
    if (('list' %in% class(scenarios)) && (length(scenarios) == 0)) {
      stop(error_codes$scenario_null, call. = F)
    } else {
      stop(error_codes$scenario_wrong_datatype, call. = F)
    }
  }
  
  # Check that it has at least one row
  if (nrow(scenarios) == 0) {
    stop(error_codes$scenario_null, call. = F)
  }
  
  # Check for missing scenario names
  missing_scenario_name <- is.na(scenarios$name) | scenarios$name == ''
  if (any(missing_scenario_name)) {
    stop(error_codes$scenario_missing_name, call. = F)
  }
  
  # Check for missing parameter names
  missing_param_names <- is.na(scenarios$param_name) | scenarios$param_name == ''
  if (any(missing_param_names)) {
    stop(
      glue(
        scenario_missing_param_name,
        scenario_name = scenarios$scenario_name[missing_param_names][1]
      ),
      call. = F
    )
  }
  
  # Check for missing parameter values
  missing_param_value <- is.na(scenarios$formula) | scenarios$formula == ''
  if (any(missing_param_value)) {
    stop(
      glue(
        missing_param_value,
        scenario_name = scenarios$scenario_name[missing_param_value][1],
        param_name = scenarios$param_name[missing_param_value][1]
      ),
      call. = F
    )
  }
  
  # Check that parameters are used no more than once per scenario
  dupe_params <- scenarios %>%
    group_by(scenario_name, param_name) %>%
    summarize(n = n()) %>%
    filter(n > 1)
  
  if (nrow(dupe_params) > 0) {
    stop(
      glue(
        error_codes$duplicate_param_in_scenario,
        scenario_name = dupe_params$scenario_name,
        param_name = dupe_params$param_name
      ),
      call. = F
    )
  }
}

scenario_format_res <- function(res, scenarios, id_vars = NULL) {
  if (is.null(id_vars)) {
    id_vars <- c('outcome', 'disc', 'series')
  }
  if ('.vbp_scen' %in% colnames(res)) {
    res <- filter(res, is.na(.vbp_scen))
  }
  scen_res_formatted <- mutate(
      res,
      scenario = ifelse(is.na(.scenario), 'Base Case', .scenario)
    ) %>%
    left_join(
      distinct(scenarios, scenario_name, description),
      by = c('scenario' = 'scenario_name')
    ) %>%
    mutate(
      description = ifelse(scenario == 'Base Case', 'Base case scenario of model.', description),
      scenario = factor(scenario, levels = unique(c("Base Case", scenarios$scenario_name)))
    ) %>%
    arrange_at(c(id_vars, 'scenario')) %>%
    group_by_at(id_vars) %>%
    group_split() %>%
    purrr::map(function(x) {
      res_list <- select(x[1,], !!id_vars) %>%
        as.list()
      res_list$data <- select(x, scenario, value, description)
      return(res_list)
    })
  
  return(scen_res_formatted)
}