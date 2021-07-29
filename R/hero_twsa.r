#' @export
run_hero_twsa <- function(...) {
  
  # Build model object
  dots <- patch_progress_funcs(list(...))
  print(dots$twsa)
  print(dots$twsa_parameters)
  print(dots$twsa_settings)
  check_dsa_vars(dots$variables)
  args <- do.call(build_hero_model, dots)
  
  max_prog <- get_dsa_max_progress(dots)
  try(dots$report_max_progress(max_prog))
  
  # Initial model run
  heemod_res <- do.call(run_model_api, args)
  vbp_name <- dots$vbp$par_name
  
  # Generate sensitvity analysis input table
  twsa_table <- gen_twsa_table(dots$twsa, dots$twsa_parameters)
  groups_table <- gen_groups_table(dots$groups)
  if (is.null(dots$twsa_settings) || !dots$twsa_settings$run_vbp) {
    vbp_table <- tibble(.vbp_scen = NA, .vbp_price = NA, .vbp_param = list(NA))
    run_vbp <- FALSE
  } else {
    vbp_table <- gen_vbp_table(dots$vbp)
    run_vbp <- TRUE
  }
  sa_table <- crossing(groups_table, twsa_table, vbp_table)
  n_row <- nrow(sa_table)
  indices <- rep(T, n_row)
  if (vbp_name %in% colnames(sa_table)) {
    indices <- !is.na(sa_table$.vbp_param)
  }
  sa_table[[vbp_name]][indices] <- sa_table$.vbp_param[indices]
  sa_table <- select(sa_table, -.vbp_param) %>%
    dplyr::relocate(.twsa_id, .x_param_name, .x_param_id, .y_param_name, .y_param_id, .x_param_name, .group_scen, .group_weight, .vbp_scen, .vbp_price)

  # Run sensitivity Analyses
  res <- run_sa(
    heemod_res$model_runs,
    sa_table, c('.twsa_id', '.x_param_name', '.x_param_id', '.y_param_name', '.y_param_id'),
    report_progress = dots$report_progress,
    heemod_res$model_runs$cores
  )

  # Pull out results for each scenario
  outcomes_res <- extract_sa_summary_res(res, dots$hsumms, c('.twsa_id', '.x_param_name', '.x_param_id', '.y_param_name', '.y_param_id'))
  costs_res <- extract_sa_summary_res(res, dots$esumms, c('.twsa_id', '.x_param_name', '.x_param_id', '.y_param_name', '.y_param_id', '.x_param_name'))
  nmb_res <- extract_sa_nmb(outcomes_res, costs_res, dots$hsumms, dots$esumms, c('.twsa_id', '.x_param_name', '.x_param_id', '.y_param_name', '.y_param_id'))
  if (run_vbp) {
    vbp_res <- extract_sa_vbp(outcomes_res, costs_res, dots$vbp, dots$hsumms, c('.twsa_id', '.x_param_name', '.x_param_id', '.y_param_name', '.y_param_id'))
  }
  
  # Format and Return
  # list(
  #   outcomes = dsa_reformat_res(outcomes_res),
  #   cost = dsa_reformat_res(costs_res),
  #   nmb = dsa_reformat_res(nmb_res, id_vars = c('health_outcome', 'econ_outcome', 'series')),
  #   vbp = if (run_vbp) list(
  #     prices = dsa_reformat_res(vbp_res, c('series')),
  #     referent = dots$vbp$strat
  #   ) else NULL,
  #   api_ver = '2.0'
  # )
}

get_twsa_param_formula <- function(param) {
  if (param$type == 'custom') {
    res <- trimws(strsplit(param$data$custom, ', ')[[1]])
  } else if (param$type == 'radius') {
    steps <- as.numeric(param$data$steps)
    index <- seq_len(steps * 2 + 1)
    res <- glue(
      'seq(from = bc - {radius}, to = bc + {radius}, length.out = {steps * 2 + 1})[{index}]',
      radius  = param$data$radius,
      steps = steps,
      index = index
    )
  } else if (param$type == 'range') {
    steps <- as.numeric(param$data$steps)
    index <- seq_len(steps)
    res <- glue(
      'seq(from = {min}, to = {max}, length.out = {steps})[{index}]',
      min = param$data$min,
      max = param$data$max,
      steps = steps,
      index = index
    )
  } else {
    stop('Invalid option selected for parameter in two-way sensitivity analysis. Must be "range", "radius", or "custom".', call. = F)
  }
  
  map(res, function(x) {
      create_sa_lazy_param(
        x,
        glue(
          'parameter {param} in two-way sensitivity analysis',
          param = param$name
        )
      )
    }
  )
}

gen_twsa_table <- function(twsa, twsa_params) {
  param_names <- unique(twsa_params$name)
  twsa_params <- twsa %>%
    rename(
      '.twsa_id' = id
    ) %>%
    left_join(rename(twsa_params, '.param_id' = id), by = c('.twsa_id' = 'parentid')) %>%
    group_by(.twsa_id) %>% 
    group_split() %>%
    map(function(analysis) {
      other_names <- setdiff(param_names, analysis$name)
      x_param_values <- get_twsa_param_formula(analysis[1, ])
      y_param_values <- get_twsa_param_formula(analysis[2, ])
      twsa_analysis_table <- expand_grid(
        x = x_param_values,
        y = y_param_values
      ) %>%
        set_colnames(analysis$name) %>%
        mutate(
          '.twsa_id' = analysis$.twsa_id[1],
          '.x_param_name' = analysis$name[1],
          '.y_param_name' = analysis$name[2],
          '.x_param_id' = analysis$.param_id[1],
          '.y_param_id' = analysis$.param_id[2]
        ) %>%
        relocate(.twsa_id, .x_param_name, .x_param_id, .y_param_name, .y_param_id, .x_param_name)
      for (name in other_names) {
        twsa_analysis_table[[name]] <- rep(list(NA), nrow(twsa_analysis_table))
      }
      
      twsa_analysis_table
    }) %>%
    bind_rows()
  
  twsa_params
  
}