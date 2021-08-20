#' @export
run_hero_twsa <- function(...) {
  
  # Build model object
  dots <- patch_progress_funcs(list(...))

  twsa_param_names <- unique(dots$twsa_parameters$name)
  check_twsa(dots$twsa, dots$twsa_parameters, dots$variables)
  args <- do.call(build_hero_model, dots)
  
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
  
  max_prog <- get_twsa_max_progress(dots, sa_table)
  try(dots$report_max_progress(max_prog))
  
  # Initial model run
  heemod_res <- do.call(run_model_api, args)
  vbp_name <- dots$vbp$par_name
  
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
    sa_table, c('.twsa_id', '.twsa_index', '.x_param_name', '.x_param_id', '.x_bc', '.y_param_name', '.y_param_id', '.y_bc'),
    report_progress = dots$report_progress,
    heemod_res$model_runs$cores
  )

  # Pull out results for each scenario
  outcomes_res <- extract_sa_summary_res(res, dots$hsumms, c('.twsa_id', '.twsa_index', '.x_param_name', '.x_param_id', '.x_bc', '.y_param_name', '.y_param_id', '.y_bc'), twsa_param_names, allowGroupStratDep = F)
  costs_res <- extract_sa_summary_res(res, dots$esumms, c('.twsa_id', '.twsa_index', '.x_param_name', '.x_param_id', '.x_bc', '.y_param_name', '.y_param_id', '.x_param_name', '.y_bc'), twsa_param_names)
  nmb_res <- extract_sa_nmb(outcomes_res, costs_res, dots$hsumms, dots$esumms, c('.twsa_id', '.twsa_index', '.x_param_name', '.x_param_id', '.x_bc', '.y_param_name', '.y_param_id', '.y_bc', twsa_param_names))
  if (run_vbp) {
    vbp_res <- extract_sa_vbp(outcomes_res, costs_res, dots$vbp, dots$hsumms, c('.twsa_id', '.twsa_index', '.x_param_name', '.x_param_id', '.x_bc', '.y_param_name', '.y_param_id', '.y_bc', twsa_param_names))
  }
  
  # Format and Return
  list(
    outcomes = twsa_reformat_res(outcomes_res, c('outcome', 'series', 'disc')),
    costs = twsa_reformat_res(costs_res, c('outcome', 'series', 'disc')),
    nmb = twsa_reformat_res(nmb_res, c('health_outcome', 'econ_outcome', 'series')),
    vbp = if (run_vbp) list(
      prices = twsa_reformat_res(vbp_res, c('series')),
      referent = dots$vbp$strat
    ) else NULL,
    api_ver = '2.0'
  )
}

is_df_with_at_least_one_row <- function(x) {
  ('data.frame' %in% class(x)) && (nrow(x) != 0)
}

check_twsa <- function(twsa, twsa_params, variables) {
  
  # Check that twsa isn't empty
  if (!is_df_with_at_least_one_row(twsa)) {
    stop(error_codes$twsa_empty, call. = F)
  }
  
  # Check that twsa has correct columns
  columns_present <- c('active', 'id') %in% colnames(twsa)
  if (!all(columns_present)) {
    stop(error_codes$twsa_bad_format, call. = F)
  }
  
  # Check that twsa_parameters isn't empty
  if (!is_df_with_at_least_one_row(twsa_params)) {
    stop(error_codes$twsa_params_empty, call. = F)
  }
  
  # Check that all parameters exist
  var_exists <- twsa_params$name %in% variables$name
  if (!all(var_exists)) {
    missing_vars <- twsa_params$name[which(!var_exists)]
    missing_var_csl <- vector_to_cs_string(missing_vars, quoted = T)
    stop(glue(error_codes$twsa_params_dne, params = missing_var_csl), call. = F)
  }
  
  # Check that no twsa parameters are orphaned
  twsa_exists <- twsa_params$parentid %in% twsa$id
  if (!all(twsa_exists)) {
    orphaned_vars <- twsa_params$name[which(!twsa_exists)]
    orphaned_csl <- vector_to_cs_string(orphaned_vars, quoted = T)
    stop(glue(error_codes$twsa_param_orphaned, params = orphaned_csl), call. = F)
  }
  
  # Check that there are exactly two twsa parameters in each twsa
  joined <- left_join(twsa_params, twsa_params, by = c('id' = 'parentid')) %>%
    group_by(parentid) %>%
    summarise(n = n())
  if (!all(joined$n == 2)) {
    stop(error_codes$twsa_wrong_no_params, call. = F)
  }
  
  # Check that type options are valid
  valid_options <- c('custom', 'radius', 'range')
  bad_type <- !twsa_params$type %in% valid_options
  if (any(bad_type)) {
    bad_type_vars <- twsa_params$name[which(bad_type)]
    bad_type_csl <- vector_to_cs_string(bad_type_vars, quoted = T)
    stop(glue(error_codes$twsa_param_bad_type, params = bad_type_csl), call. = F)
  }
  
}

get_twsa_param_formula <- function(param) {
  if (param$type == 'custom') {
    if (is.na(param$data$custom)) {
      stop(glue(error_codes$params_missing_value, param = param$name), call. = F)
    }
    res <- trimws(strsplit(param$data$custom, ',')[[1]])
  } else if (param$type == 'radius') {
    steps <- as.numeric(param$data$steps)
    index <- seq_len(steps * 2 + 1)
    if (is.na(param$data$radius) || is.na(param$data$steps)) {
      stop(glue(error_codes$params_missing_value, param = param$name), call. = F)
    }
    res <- glue(
      'seq(from = bc - {radius}, to = bc + {radius}, length.out = {steps * 2 + 1})[{index}]',
      radius  = param$data$radius,
      steps = steps,
      index = index
    )
  } else if (param$type == 'range') {
    if (is.na(param$data$min) || is.na(param$data$max) || is.na(param$data$steps)) {
      stop(glue(error_codes$params_missing_value, param = param$name), call. = F)
    }
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
  ) %>%
    append(NA)
}

gen_twsa_table <- function(twsa, twsa_params) {
  param_names <- unique(twsa_params$name)
  twsa_params <- twsa %>%
    rename(
      '.twsa_id' = id
    ) %>%
    filter(active == 'On' | active == TRUE) %>%
    left_join(rename(twsa_params, '.param_id' = id), by = c('.twsa_id' = 'parentid')) %>%
    group_by(.twsa_id) %>% 
    group_split() %>%
    map(function(analysis) {
      analysis <- arrange(analysis, axis)
      other_names <- setdiff(param_names, analysis$name)
      x_param_values <- get_twsa_param_formula(analysis[1, ])
      y_param_values <- get_twsa_param_formula(analysis[2, ])
      twsa_analysis_table <- expand_grid(
        x = x_param_values,
        y = y_param_values
      ) %>%
        mutate('.x_bc' = map_lgl(x, is_empty_sa_var)) %>%
        mutate('.y_bc' = map_lgl(y, is_empty_sa_var)) %>%
        set_colnames(c(analysis$name, '.x_bc', '.y_bc')) %>%
        mutate(
          '.twsa_id' = analysis$.twsa_id[1],
          '.x_param_name' = analysis$name[1],
          '.y_param_name' = analysis$name[2],
          '.x_param_id' = analysis$.param_id[1],
          '.y_param_id' = analysis$.param_id[2],
          '.twsa_index' = seq_len(n())
        ) %>%
        relocate(.twsa_id, .x_param_name, .x_param_id, .x_bc, .y_param_name, .y_param_id, .y_bc)
      for (name in other_names) {
        twsa_analysis_table[[name]] <- rep(list(NA), nrow(twsa_analysis_table))
      }
      
      twsa_analysis_table
    }) %>%
    bind_rows()
  
  twsa_params
  
}

twsa_reformat_res <- function(res, id_vars = NULL) {
  
  if ('.vbp_scen' %in% colnames(res)) {
    res <- filter(res, is.na(.vbp_scen))
  }

  res %>%
    group_by(.x_param_name) %>%
    mutate(.x = eval(parse(text = .x_param_name))) %>%
    group_by(.y_param_name) %>%
    mutate(.y = eval(parse(text = .y_param_name))) %>%
    ungroup() %>%
    select(
      id = .twsa_id,
      xParam = .x_param_name,
      yParam = .y_param_name,
      !!id_vars, 
      x = .x,
      y = .y,
      .x_bc,
      .y_bc,
      value = value
    ) %>%
    arrange_at(c('id', 'xParam', 'yParam', id_vars, 'x', 'y')) %>%
    group_by_at(c('id', 'xParam', 'yParam', id_vars)) %>%
    mutate(
      .x_bc_value = x[which(.x_bc)[1]],
      .x_equals_bc = is_zero(x - .x_bc_value),
      .y_bc_value = y[which(.y_bc)[1]],
      .y_equals_bc = is_zero(y - .y_bc_value),
      insertedBC = .x_bc | .y_bc,
      isBaseCase = .x_equals_bc & .y_equals_bc
    ) %>%
    group_split() %>%
    map(function(analysis) {
      x_bc_included <- any(analysis$.x_equals_bc & !analysis$.x_bc)
      y_bc_included <- any(analysis$.y_equals_bc & !analysis$.y_bc)
      res_list <- select(analysis[1,], id, xParam, yParam, !!id_vars) %>%
        as.list()
      res_list$data <- analysis %>%
        filter(
          !(x_bc_included & .x_bc),
          !(y_bc_included & .y_bc)
        ) %>%
        select(-.x_bc, -.y_bc, -.x_bc_value, -.x_equals_bc, -.y_bc_value, -.y_equals_bc, !!-id_vars, -id, -xParam, -yParam)
      return(res_list)
    })
}