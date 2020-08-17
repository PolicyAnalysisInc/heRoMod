#' @export
run_hero_dsa <- function(...) {
  
  # Build model object
  dots <- list(...)
  check_dsa_vars(dots$variables)
  args <- do.call(build_hero_model, dots)
  
  # Initial model run
  heemod_res <- do.call(run_model_api, args)
  vbp_name <- dots$vbp$par_name
  
  # Generate sensitvity analysis input table
  dsa_table <- gen_dsa_table(dots$variables)
  groups_table <- gen_groups_table(dots$groups)
  if (is.null(dots$dsa_settings) || !dots$dsa_settings$run_vbp) {
    vbp_table <- tibble(.vbp_scen = NA, .vbp_price = NA, .vbp_param = list(NA))
    run_vbp <- FALSE
  } else {
    vbp_table <- gen_vbp_table(dots$vbp)
    run_vbp <- TRUE
  }
  sa_table <- crossing(groups_table, dsa_table, vbp_table)
  n_row <- nrow(sa_table)
  indices <- rep(T, n_row)
  if (vbp_name %in% colnames(sa_table)) {
    indices <- !is.na(sa_table$.vbp_param)
  }
  sa_table[[vbp_name]][indices] <- sa_table$.vbp_param[indices]
  sa_table <- select(sa_table, -.vbp_param) %>%
    dplyr::relocate(.dsa_param, .dsa_side, .group_scen, .group_weight, .vbp_scen, .vbp_price)
  
  # Run sensitivity Analyses
  res <- run_sa(heemod_res$model_runs, sa_table, c('.dsa_param', '.dsa_side'))
  
  # Pull out results for each scenario
  outcomes_res <- extract_sa_summary_res(res, dots$hsumms, c('.dsa_param', '.dsa_side'))
  costs_res <- extract_sa_summary_res(res, dots$esumms, c('.dsa_param', '.dsa_side'))
  nmb_res <- extract_sa_nmb(outcomes_res, costs_res, dots$hsumms, dots$esumms, c('.dsa_param', '.dsa_side'))
  if (run_vbp) {
    vbp_res <- extract_sa_vbp(outcomes_res, costs_res, dots$vbp, dots$hsumms, c('.dsa_param', '.dsa_side'))
  }
  
  # Format and Return
  list(
    outcomes = dsa_reformat_res(outcomes_res),
    cost = dsa_reformat_res(costs_res),
    nmb = dsa_reformat_res(nmb_res, id_vars = c('health_outcome', 'econ_outcome', 'series')),
    vbp = if (run_vbp) dsa_reformat_res(vbp_res, c('series')) else NULL,
    api_ver = '2.0'
  )
}

dsa_reformat_res <- function(res, id_vars = NULL) {
  if (is.null(id_vars)) {
    id_vars <- c('outcome', 'disc', 'series')
  }
  if ('.vbp_scen' %in% colnames(res)) {
    res <- filter(res, is.na(.vbp_scen))
  }
  bc_res <- filter(res, is.na(.dsa_param)) %>%
    mutate(base = value) %>%
    select(!!id_vars, base)
  dsa_res <- filter(res, !is.na(.dsa_param)) %>%
    spread(.dsa_side, value) %>%
    left_join(bc_res, by = id_vars) %>%
    mutate(param = .dsa_param) %>%
    select(!!id_vars, param, high, low, base) %>%
    group_by_at(vars(one_of(id_vars))) %>%
    group_split() %>%
    purrr::map(function(x) {
      res_list <- select(x[1,], !!id_vars) %>%
        as.list()
      res_list$data <- select(x, !!-id_vars)
      return(res_list)
    })
  
  return(dsa_res)
}

gen_dsa_table <- function(params) {
  
  # Extract only parameters that are varied in DSA
  dsa_params <- params[params$low != '' & params$high != '', ]
  param_names <- dsa_params$name
  n_params <- length(param_names)
  
  # Create a table to store the parameter values to use in each iteration
  dsa_table <- create_sa_table((n_params * 2) + 1, n_params, param_names)
  
  # Populate the table with low/high parameter values
  for (i in seq_len(n_params)) {
    high_row <- (i * 2) + 1
    low_row <- high_row - 1
    
    # Need to add error handling here
    dsa_table[[i]][[low_row]] <- as.lazy(dsa_params$low[i])
    dsa_table[[i]][[high_row]] <- as.lazy(dsa_params$high[i])
  }
  
  dsa_table <- mutate(
    dsa_table,
    .dsa_param = c(NA, rep(param_names, each = 2)),
    .dsa_side = c(NA, rep(c("low", "high"), n_params))
  ) %>%
    dplyr::relocate(.dsa_param, .dsa_side)
  
  return(dsa_table)
  
}
