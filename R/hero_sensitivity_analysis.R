
run_sa <- function(model, scenarios, group_vars, report_progress = identity, create_progress_reporter = create_null_prog_reporter, cores = cores_to_use(), simplify = F) {
  
  answer_key <- select(scenarios, !!group_vars, .group_scen, .vbp_scen, .vbp_price)
  inputs <- select(scenarios, -!!group_vars, -.group_scen, -.vbp_scen, -.vbp_price)
  
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
      cores = cores,
      report_progress = report_progress,
      create_progress_reporter = create_progress_reporter,
      simplify = simplify
    )
    res$series <- strat_name
    res$.group_weight <- map_dbl(res$.mod, function(x) x$parameters$.group_weight[1])
    bind_cols(answer_key, res)
  }) %>%
    bind_rows()
  res
}

extract_sa_outcome <- function(y, summaries) {
  totals <- as.numeric(colSums(y$values[ , -1]))
  total_df <- tibble(
    outcome = colnames(y$values)[-1],
    value = totals
  )
  return(total_df)
}

extract_sa_nmb <- function(outcomes, costs, health_summaries, economic_summaries, group_vars) {
  hsumm_unique <- distinct(health_summaries, name, .keep_all = T) %>%
    select(name, wtp)
  outcomes_summ <- filter(outcomes, outcome %in% health_summaries$name, disc) %>%
    left_join(hsumm_unique, by = c('outcome' = 'name')) %>%
    mutate(
      health_outcome = outcome,
      health_nmb = value * wtp
    ) %>%
    select(series, !!group_vars, .vbp_scen, .vbp_price, health_outcome, health_nmb)

  costs_summ <- filter(costs, outcome %in% economic_summaries$name, disc) %>%
    mutate(
      econ_outcome = outcome,
      econ_nmb = -value
    ) %>%
    select(series, !!group_vars, .vbp_scen, .vbp_price, econ_outcome, econ_nmb)
  
  nmb_res <- crossing(
    health_outcome = unique(health_summaries$name),
    econ_outcome = unique(economic_summaries$name)
  ) %>%
    inner_join(outcomes_summ, by = "health_outcome") %>%
    left_join(costs_summ, by = c("econ_outcome", "series", group_vars, ".vbp_scen", ".vbp_price")) %>%
    mutate(value = health_nmb + econ_nmb) %>%
    select(series, !!group_vars, .vbp_scen, .vbp_price, health_outcome, econ_outcome, value)
  
  return(nmb_res)
}

extract_sa_vbp <- function(outcomes, costs, vbp, hsumm, group_vars) {
  
  # Pull out parameters of VBP analysis
  vbp_strat <- vbp$strat
  vbp_hsumm <- vbp$effect
  vbp_esumm <- vbp$cost
  wtp <- as.numeric(filter(hsumm, name == vbp_hsumm)$wtp[1])
  
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
      vbp_value <- calculate_vbp(wtp, vbp_eq$intercept, vbp_eq$slope)
      res_df <- tibble(
        value = vbp_value,
        slope = vbp_eq$slope,
        intercept = vbp_eq$intercept
      )
      res_df
    }) %>%
    ungroup() %>%
    select(!!group_vars, series, value, slope, intercept)
  
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
      series = comp_name,
      value = ref_value - comp_value
    )
}

extract_sa_summary_res <- function(results, summaries, group_vars, vars_to_include = c(), allowGroupStratDep = T) {
  
  summary_res <- results %>%
    select_at(c('series', group_vars, '.vbp_scen', '.vbp_price', '.mod', '.group_weight')) %>%
    rowwise() %>%
    group_split() %>%
    map(function(x) {
      bind_cols(
      x,
      extract_sa_outcome(x$.mod[[1]], summaries),
      extract_parameter_values(x$.mod[[1]], vars_to_include)
    )}) %>%  # Extract outcomes results
    bind_rows() %>%
    mutate(
      disc = substring(outcome, 1, 6) == '.disc_',
      outcome = ifelse(disc, substring(outcome, 7), outcome)
    ) %>% # Properly label discounted results
    filter(outcome %in% c(summaries$name, summaries$value)) %>% # only keep results related to relevant values/summaries
    (function(x) {
      if (!allowGroupStratDep) check_sa_params(x, group_vars, vars_to_include)
      else x
    })%>%
    group_by_at(c('series', group_vars, '.vbp_scen', '.vbp_price', 'outcome', 'disc', vars_to_include)) %>%
    summarize(value = sum(value * .group_weight/sum(.group_weight))) %>% # aggregate by group
    ungroup()
  return(summary_res)
}

check_sa_params <- function(res, group_vars, vars_to_include) {
  df1 <- distinct_at(res, c(group_vars, '.vbp_scen', '.vbp_price'))
  df2 <- distinct_at(res, c(group_vars, '.vbp_scen', '.vbp_price', vars_to_include))
  if (nrow(df1) != nrow(df2)) {
    stop(error_codes$twsa_group_strat_dep, call. = F)
  }
  res
}

gen_groups_table <- function(groups) {
  
  if (length(groups) == 0) {
    # Handle models with no groups by using a single dummy group
    return(
      tibble(
        .group_scen = 'All Patients',
        .group_weight = '1'
      )
    )
  }
  
  attribs <- groups %>%
    rename(.group_weight = weight) %>%
    mutate(group = name, .group = name) %>%
    select(-name) %>%
    select(group, .group, everything())
  n_groups <- nrow(attribs)
  n_params <- ncol(attribs)
  param_names <- colnames(attribs)
  
  # Create a table to store the parameter values to use in each group
  groups_table <- create_sa_table(n_groups, n_params, param_names)
  for (i in seq_len(n_params)) {
    # Needs error handling
    var_list <- as.lazy_dots(as.character(attribs[[i]]))
    class(var_list) <- 'list'
    groups_table[[i]] <- var_list
  }
  groups_table$.group_scen <- gsub('"', '', groups$name, fixed= T)
  groups_table <- dplyr::relocate(groups_table, .group_scen)
  return(groups_table)
}

create_sa_table <- function(n_scen, n_par, par_names) {
  blank_col <- tibble(rep(list(NA), length = n_scen))
  sa_table <- blank_col[ , rep(1, n_par)]
  colnames(sa_table) <- par_names
  return(sa_table)
}

extract_parameter_values <- function(res, params) {
  if (is.null(params)) {
    return(params[1, c()])
  }
  res$parameters[1, params]
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
          lazy_param$env$bc <- strat$parameters[[var_names[i]]][1]
        }
        lazy_param
    })
  }
  return(strat_scenarios)
}

create_sa_lazy_param <- function(x, context = '') {
  tryCatch({
    lazy <- as.lazy(x)
  }, error = function(err) {
    stop(glue(error_codes$syntax_error, context = context), call. = FALSE)
  })
  lazy$env <- new.env(parent = parent.env(globalenv()))
  lazy
}