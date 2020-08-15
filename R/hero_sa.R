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
    vbp_table <- gen_vbp_table()
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
  n_cores <- 16#max(1, round((parallel::detectCores() - 2)/3, 0))
  res <- run_sa(heemod_res$model_runs, sa_table, n_cores)
  
  # Pull out results for each scenario
  outcomes_res <- extract_sa_summary_res(res, dots$hsumms)
  costs_res <- extract_sa_summary_res(res, dots$esumms)
  nmb_res <- extract_sa_nmb(outcomes_res, costs_res, dots$hsumms, dots$esumms)
  if (run_vbp) {
    vbp_res <- extract_sa_vbp(outcomes_res, costs_res, dots$vbp, c('.dsa_param', '.dsa_side'))
  }
  
  # Format and Return
  list(
    outcomes = dsa_reformat_res(outcomes_res),
    cost = dsa_reformat_res(costs_res),
    nmb = dsa_reformat_res(nmb_res, id_vars = c('health_outcome', 'econ_outcome', 'series')),
    vbp = if (run_vbp) dsa_reformat_res(vbp_res, c('series')) else NULL
  )
}

extract_sa_outcome <- function(y, summaries) {
  totals <- as.numeric(colSums(y$values[ , -1]))
  total_df <- tibble(
    outcome = colnames(y$values)[-1],
    value = totals
  )
  return(total_df)
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

extract_sa_nmb <- function(outcomes, costs, health_summaries, economic_summaries) {
  hsumm_unique <- distinct(health_summaries, name, .keep_all = T) %>%
    select(name, wtp)
  outcomes_summ <- filter(outcomes, outcome %in% health_summaries$name, disc) %>%
    left_join(hsumm_unique, by = c('outcome' = 'name')) %>%
    mutate(
      health_outcome = outcome,
      health_nmb = value * wtp
    ) %>%
    select(series, .dsa_param, .dsa_side, .vbp_scen, .vbp_price, health_outcome, health_nmb)

  costs_summ <- filter(costs, outcome %in% economic_summaries$name, disc) %>%
    mutate(
      econ_outcome = outcome,
      econ_nmb = -value
    ) %>%
    select(series, .dsa_param, .dsa_side, .vbp_scen, .vbp_price, econ_outcome, econ_nmb)
  
  nmb_res <- crossing(
    health_outcome = unique(health_summaries$name),
    econ_outcome = unique(economic_summaries$name)
  ) %>%
    inner_join(outcomes_summ, by = "health_outcome") %>%
    left_join(costs_summ, by = c("econ_outcome", "series", ".dsa_param", ".dsa_side", ".vbp_scen", ".vbp_price")) %>%
    mutate(value = health_nmb + econ_nmb) %>%
    select(series, .dsa_param, .dsa_side, .vbp_scen, .vbp_price, health_outcome, econ_outcome, value)
  
  return(nmb_res)
}

extract_sa_vbp <- function(outcomes, costs, vbp, group_vars) {
  
  # Pull out parameters of VBP analysis
  vbp_strat <- vbp$strat
  vbp_hsumm <- vbp$effect
  vbp_esumm <- vbp$cost
  
  # Calculate the difference in outcomes for each scenario
  outcome_res_vs_ref <- filter(outcomes, disc, outcome == vbp_hsumm, !is.na(.vbp_scen)) %>%
    select(-disc) %>%
    calc_dsa_deltas_vs_ref(vbp_strat, c(group_vars, '.vbp_scen', '.vbp_price')) %>%
    mutate(delta_outcome = value) %>%
    select(!!group_vars, series, .vbp_scen, .vbp_price, delta_outcome)
  
  # Calculate the difference in outcomes for each scenario
  cost_res_vs_ref <- filter(costs, disc, outcome == vbp_esumm, !is.na(.vbp_scen)) %>%
    select(-disc) %>%
    calc_dsa_deltas_vs_ref(vbp_strat, c(group_vars, '.vbp_scen', '.vbp_price')) %>%
    mutate(delta_cost = value) %>%
    select(!!group_vars, series, .vbp_scen, .vbp_price, delta_cost)
  
  # Join costs and outcomes results
  res_vs_ref <- left_join(
    outcome_res_vs_ref,
    cost_res_vs_ref,
    by = c(group_vars, 'series', '.vbp_scen', '.vbp_price')
  )
  
  # Calculate VBP for each analysis
  vbp_res <- res_vs_ref %>%
    group_by_at(c(group_vars, 'series')) %>%
    do({
      vbp_eq <- calculate_vbp_equation(.$.vbp_price, .$delta_cost, .$delta_outcome)
      vbp_value <- calculate_vbp(as.numeric(vbp$wtp), vbp_eq$intercept, vbp_eq$slope)
      res_df <- tibble(
        value = vbp_value,
        slope = vbp_eq$slope,
        intercept = vbp_eq$intercept
      )
      res_df
    }) %>%
    ungroup() %>%
    select(!!group_vars, series, value)
  
  return(vbp_res)
}

# Given a set of sensitivity analysis results, calculate the difference
# in those results vs. a reference strategy.
calc_dsa_deltas_vs_ref <- function(results, referent, id_vars) {
  outcome_res_ref <- filter(results, series == referent) %>%
    mutate(ref_value = value, ref_name = series) %>%
    select(-value, -series)
  outcome_res_comp <- filter(results, series != referent) %>%
    mutate(comp_value =  value, comp_name = series) %>%
    select(-value, -series)
  outcome_res_ref_vs_comp <- outcome_res_comp %>%
    left_join(outcome_res_ref, by = id_vars) %>%
    mutate(
      series = paste0(referent, ' vs. ', comp_name),
      value = ref_value - comp_value
    )
}

extract_sa_summary_res <- function(results, summaries) {
  summary_res <- results %>%
    #filter(is.na(.vbp_scen)) %>%    # Don't need the VBP scenarios to calculate outcomes
    rowwise() %>%
    group_split() %>%
    map(function(x) bind_cols(x, extract_sa_outcome(x$.mod[[1]], summaries))) %>%  # Extract outcomes results
    bind_rows() %>%
    mutate(
      disc = substring(outcome, 1, 6) == '.disc_',
      outcome = ifelse(disc, substring(outcome, 7), outcome)
    ) %>% # Properly label discounted results
    filter(outcome %in% c(summaries$name, summaries$value)) %>% # only keep results related to relevant values/summaries
    group_by(series, .dsa_param, .dsa_side, .vbp_scen, .vbp_price, outcome, disc) %>%
    summarize(value = sum(value * .group_weight/sum(.group_weight))) %>% # aggregate by group
    ungroup()
  
  return(summary_res)
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

gen_groups_table <- function(groups) {
  
  if (length(groups) == 0) {
    # Handle models with no groups by using a single dummy group
    return(
      tibble(
        .group_scen = 'All Patients',
        .group_weight = 1
      )
    )
  }
  
  attribs <- groups %>%
    mutate(group = name, .group = name) %>%
    select(-name, -weight) %>%
    select(group, .group, everything())
  n_groups <- nrow(attribs)
  n_params <- ncol(attribs)
  param_names <- colnames(attribs)
  
  # Create a table to store the parameter values to use in each group
  groups_table <- create_sa_table(n_groups, n_params, param_names)
  for (i in seq_len(n_params)) {
    # Needs error handling
    var_list <- as.lazy_dots(attribs[[i]])
    class(var_list) <- 'list'
    groups_table[[i]] <- var_list
  }
  groups_table$.group_scen <- gsub('"', '', groups$name, fixed= T)
  groups_table$.group_weight <- as.numeric(groups$weight)
  groups_table <- dplyr::relocate(groups_table, .group_scen, .group_weight)
  return(groups_table)
}


create_sa_table <- function(n_scen, n_par, par_names) {
  blank_col <- tibble(rep(list(NA), length = n_scen))
  sa_table <- blank_col[ , rep(1, n_par)]
  colnames(sa_table) <- par_names
  return(sa_table)
}

combine_sa_tables <- function(sa, groups, vbp) {
  
}

run_sa <- function(model, scenarios, cores) {
  
  answer_key <- select(scenarios, .dsa_param, .dsa_side, .group_scen, .group_weight, .vbp_scen, .vbp_price)
  inputs <- select(scenarios, -.dsa_param, -.dsa_side, -.group_scen, -.group_weight, -.vbp_scen, -.vbp_price)
  
  strategy_names <- get_strategy_names(model)
  var_names <- colnames(scenarios)
  n_strategies <- length(strategy_names)
  n_scen <- nrow(scenarios)
  n_param <- ncol(scenarios)
  res <- map(seq_len(n_strategies), function(i) {
    strat_name <- strategy_names[i]
    strat_scenarios <- populate_bc(model$eval_strategy_list[[i]], inputs)
    res <- eval_strategy_newdata(
      model,
      strategy = strat_name,
      newdata = strat_scenarios,
      cores = cores
    )
    res$series <- strat_name
    bind_cols(answer_key, res)
  }) %>%
    bind_rows()
  res
}

populate_bc <- function(strat, scenarios) {
  n_scen <- nrow(scenarios)
  n_param <- ncol(scenarios)
  var_names <- colnames(scenarios)
  strat_scenarios <- scenarios
  for (i in seq_len(n_param)) {
      strat_scenarios[[i]] <- map(seq_len(n_scen), function(j) {
        lazy_param <- strat_scenarios[[i]][[j]]
        if ("lazy" %in% class(strat_scenarios[[i]][[j]])) {
          lazy_param$env <- new.env(parent = strat_scenarios[[i]][[j]]$env)
          lazy_param$env$bc <- strat$parameters[[var_names[i]]]
        }
        lazy_param
    })
  }
  return(strat_scenarios)
}