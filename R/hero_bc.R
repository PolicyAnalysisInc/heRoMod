#' @export
run_hero_bc <- function(...) {
  
  # Build model object
  dots <- patch_progress_funcs(list(...))
  args <- do.call(build_hero_model, dots)
  
  max_prog <- get_dsa_max_progress(dots)
  try(dots$report_max_progress(max_prog))
  
  # Initial model run
  heemod_res <- do.call(run_model_api, args)
  vbp_name <- dots$vbp$par_name
  
  if ((class(dots$groups) %in% "data.frame") && (nrow(dots$groups) > 1)) {
    
    # Generate sensitvity analysis input table
    groups_table <- gen_groups_table(dots$groups)
    vbp_table <- tibble(.vbp_scen = NA, .vbp_price = NA, .vbp_param = list(NA))
    sa_table <- crossing(groups_table, vbp_table)
    n_row <- nrow(sa_table)
    
    # Run sensitivity Analyses
    res <- run_sa(
      heemod_res$model_runs,
      sa_table, c(),
      report_progress = dots$report_progress,
      heemod_res$model_runs$cores
    )
    
  } else {
    strat_list <- heemod_res$model_runs$eval_strategy_list
    strat_names <- names(strat_list)
    res <- tibble(
      series = strat_names,
      .mod = strat_list,
      .group_weight = rep(1, length(strat_names))
    )
  }
  
  # Pull out results for each scenario
  trace_res <- extract_sa_bc_trace(res)
  outcome_res <- extract_sa_bc_summaries(res, dots$hsumms)
  costs_res <- extract_sa_bc_summaries(res, dots$esumms)
  ce_res <- extract_sa_bc_incrmental_ce(outcome_res, costs_res)
  pw_ce_res <- extract_sa_bc_pairwise_ce(outcome_res, costs_res)
  nmb_res <- extract_sa_bc_nmb(outcome_res, costs_res, dots$hsumms)
  
  # Format and Return
  list(
    trace = trace_res,
    outcomes = outcome_res,
    costs = costs_res,
    ce = ce_res,
    pairwise_ce = pw_ce_res,
    nmb = nmb_res,
    api_ver = '2.0'
  )
}

extract_sa_bc_trace <- function(res, corrected = F) {
  strat_order <- unique(res$series)
  time_cols <- res$.mod[[1]]$parameters %>%
    distinct(model_day, model_week, model_month, model_year)
  if (!corrected) {
    time_cols <- rbind(data.frame(model_day=0,model_week=0,model_month=0,model_year=0), time_cols, stringsAsFactors = F)
  }
  trace <- res %>%
    group_by(series) %>%
    group_split() %>%
    map(function(strat_res) {
      sum_weights <- sum(strat_res$.group_weight)
      weighted_trace <- map2(strat_res$.mod, strat_res$.group_weight, function(group_res, weight) {
        if (corrected) trace <- group_res$counts
        else trace <- group_res$counts_uncorrected
        trace * (weight / sum_weights)
      }) %>% do.call(`+`, .)
      cbind(series = strat_res$series[1], time_cols, weighted_trace, stringsAsFactors = F)
    }) %>%
    bind_rows() %>%
    arrange(factor(series, levels = strat_order), model_day) %>%
    relocate(model_day, model_week, model_month, model_year, series)
  
  trace
}
extract_sa_bc_summaries <- function(res, summaries) {
  
  strategy_names <- unique(res$series)
  summary_names <- unique(summaries$name)
  value_names <- unique(summaries$value)
  value_names_disc <- to_disc_name(value_names)
  n_summ_rows <- nrow(summaries)
  comparisons <- crossing(
    referent = strategy_names,
    comparator = strategy_names
  ) %>%
    filter(referent != comparator) %>%
    mutate(series = to_comparison_name(referent, comparator)) %>%
    arrange(factor(comparator, levels = strategy_names), factor(referent, levels = strategy_names))
  
  series_levels <- c(strategy_names, comparisons$series)
  
  abs_res <- res %>%
    group_by(series) %>%
    group_split() %>%
    map(function(strat_res) {
      map2(strat_res$.mod, strat_res$.group_weight, function(group_res, weight) {
        names_in_df <- c(value_names, value_names_disc)[c(value_names, value_names_disc) %in% colnames(group_res$values)]
        n_values <- length(names_in_df)
        values_res <- tibble(
          group = to_undisc_name(names_in_df),
          value = colSums(group_res$values[ , names_in_df])
        ) %>%
          mutate(
            series = strat_res$series[1],
            disc = rep(c(F, T), each = n_values / 2)
          )
        
        summ_res <- rbind(
          summaries,
          summaries,
          stringsAsFactors = F
        ) %>%
          filter(value %in% names_in_df) %>%
          mutate(disc = rep(c(F, T), each = n()/2)) %>%
          rename(outcome = name, group = value) %>%
          left_join(values_res, by = c('group', 'disc')) %>%
          select(outcome, series, group, disc, value) %>%
          mutate(.group_weight = weight)
        
        summ_res
      }) %>%
        bind_rows()
    }) %>%
    bind_rows()  %>%
    group_by(outcome, series, group, disc) %>%
    summarize(value = sum(.group_weight * value) / sum(.group_weight)) %>%
    ungroup()
  
  delta_res <- comparisons %>%
    inner_join(
      rename(abs_res, ref_value = value),
      by = c('referent' = 'series')
    ) %>%
    inner_join(
      rename(abs_res, comp_value = value),
      by = c('comparator' = 'series', 'outcome', 'group', 'disc')
    ) %>%
    mutate(value = ref_value - comp_value) %>%
    select(outcome, series, group, disc, value)
  
  all_res <- rbind(abs_res, delta_res, stringsAsFactors = F) %>%
    arrange(
      -disc,
      factor(outcome, levels = summary_names),
      factor(group, levels = value_names),
      factor(series, levels = series_levels)
    ) %>%
    as.data.frame()
  
  all_res
}
extract_sa_bc_incrmental_ce <- function(outcomes, costs) {
  
  outcome_names <- unique(outcomes$outcome)
  disc_outcome_names <- to_disc_name(outcome_names)
  cost_names <- unique(costs$outcome)
  disc_cost_names <- to_disc_name(cost_names)
  strategy_names <- unique(outcomes$series[!is_comparison_name(outcomes$series)])
  
  pairs <- crossing(health_outcome = outcome_names, econ_outcome = cost_names)
  
  outcomes_res_total <- outcomes %>%
    filter(disc, series %in% strategy_names) %>%
    group_by(outcome, series) %>%
    summarize(.effect = sum(value))
  
  costs_res_total <- costs %>%
    filter(disc, series %in% strategy_names) %>%
    group_by(outcome, series) %>%
    summarize(.cost = sum(value)) %>%
    ungroup()
  
  ce_res <- pairs %>%
    inner_join(outcomes_res_total, by = c('health_outcome' = 'outcome')) %>%
    inner_join(costs_res_total, by = c('econ_outcome' = 'outcome', 'series' = 'series')) %>%
    mutate(.strategy_names = series) %>%
    arrange(.cost) %>%
    group_by(health_outcome, econ_outcome) %>%
    do(compute_icer(., strategy_order = seq_len(nrow(.)))) %>%
    mutate(
      health_outcome = to_disc_name(health_outcome),
      hsumm = health_outcome,
      econ_outcome = to_disc_name(econ_outcome),
      esumm = econ_outcome,
      cost = .cost,
      eff = .effect,
      dcost = .dcost,
      deffect = .deffect,
      dref = .dref,
      icer = .icer
    ) %>%
    select(hsumm, esumm, health_outcome, econ_outcome, series, cost, eff, dcost, deffect, dref, icer) %>%
    arrange(
      factor(hsumm, levels = disc_outcome_names),
      factor(esumm, levels = disc_cost_names),
      factor(cost)
    ) %>%
    as.data.frame()
    
  ce_res
}
extract_sa_bc_pairwise_ce <- function(outcomes, costs) {
  
  outcome_names <- unique(outcomes$outcome)
  cost_names <- unique(costs$outcome)
  strategy_names <- unique(outcomes$series[!is_comparison_name(outcomes$series)])
  analyses <- crossing(
    health_outcome = outcome_names,
    econ_outcome = cost_names,
    referent = strategy_names,
    comparator = strategy_names
  ) %>%
    filter(referent != comparator) %>%
    mutate(series = to_comparison_name(referent, comparator))
  
  outcomes_res_total <- outcomes %>%
    filter(disc, series %in% strategy_names) %>%
    group_by(outcome, series) %>%
    summarize(effect = sum(value))
  
  costs_res_total <- costs %>%
    filter(disc, series %in% strategy_names) %>%
    group_by(outcome, series) %>%
    summarize(cost = sum(value)) %>%
    ungroup()
  
  ce_res <- analyses %>%
    inner_join(
      rename(outcomes_res_total, ref_effect = effect),
      by = c('health_outcome' = 'outcome', 'referent' = 'series')
    ) %>%
    inner_join(
      rename(outcomes_res_total, comp_effect = effect),
      by = c('health_outcome' = 'outcome', 'comparator' = 'series')
    ) %>%
    inner_join(
      rename(costs_res_total, ref_cost = cost),
      by = c('econ_outcome' = 'outcome', 'referent' = 'series')
    ) %>%
    inner_join(
      rename(costs_res_total, comp_cost = cost),
      by = c('econ_outcome' = 'outcome', 'comparator' = 'series')
    ) %>%
    transmute(
      health = health_outcome,
      econ = econ_outcome,
      series,
      referent,
      comparator,
      effect = comp_effect,
      cost = comp_cost,
      deffect = ref_effect - comp_effect,
      dcost = ref_cost - comp_cost,
      icer = compute_pw_icer(deffect, dcost),
      icer_string = format_icer(icer)
    ) %>%
    arrange(
      health,
      econ,
      referent,
      comparator
    ) %>%
    as.data.frame()
  
  ce_res
}
extract_sa_bc_nmb <- function(outcomes, costs, hsumms) {
  
  thresholds <- select(distinct(hsumms, name, .keep_all = T), name, wtp)
  
  outcome_names <- unique(outcomes$outcome)
  cost_names <- unique(costs$outcome)
  strategy_names <- unique(outcomes$series[is_comparison_name(outcomes$series)])
  
  outcomes_res_total <- outcomes %>%
    filter(disc, series %in% strategy_names) %>%
    left_join(thresholds, by = c('outcome' = 'name')) %>%
    mutate(value = value * wtp, type = 'health') %>%
    select(-wtp)
  
  costs_res_total <- costs %>%
    filter(disc, series %in% strategy_names) %>%
    mutate(value = -value, type = 'economic')
  
  nmb_res <- rbind(outcomes_res_total, costs_res_total, stringsAsFactors = F) %>%
    select(outcome, series, group, disc, type, value)
  
  nmb_res
}