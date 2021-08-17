#' @export
export_hero_xlsx <- function(...) {
  # Build model object
  dots <- patch_progress_funcs(list(...))
  args <- do.call(build_hero_model, dots)
  
  max_prog <- get_excel_max_progress(dots)
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
  trace_res <- extract_sa_bc_trace(res, corrected = F)
  cor_trace_res <- extract_sa_bc_trace(res, corrected = T)
  outcomes_res <- extract_sa_bc_summaries(res, dots$hsumms)
  costs_res <- extract_sa_bc_summaries(res, dots$esumms)
  ce_res <- extract_sa_bc_incrmental_ce(outcomes_res, costs_res)
  pw_ce_res <- extract_sa_bc_pairwise_ce(outcomes_res, costs_res)
  nmb_res <- extract_sa_bc_nmb(outcomes_res, costs_res, dots$hsumms)
    
  
  outcome_res_xlsx <- outcomes_res %>%
    rename(
      "Outcome" = outcome,
      "Strategy" = series,
      "Component" = group,
      "Discounted" = disc,
      "Value" = value
    )
  costs_res_xlsx <- costs_res %>%
    rename(
      "Outcome" = outcome,
      "Strategy" = series,
      "Component" = group,
      "Discounted" = disc,
      "Value" = value
    )
  nmb_res_xlsx <- nmb_res %>%
    rename(
      "Outcome" = outcome,
      "Strategy" = series,
      "Component" = group,
      "Type" = type,
      "Value" = value
    ) %>%
    select(-disc)
  
  ce_res_xlsx <- ce_res %>%
    mutate(
      health_outcome = substring(health_outcome, 7),
      econ_outcome = substring(econ_outcome, 7)
    ) %>%
    rename(
      "Health Outcome" = health_outcome,
      "Economic Outcome" = econ_outcome,
      "Strategy" = series,
      "Cost" = cost,
      "Effect" = eff,
      "\u0394 Cost" = dcost,
      "\u0394 Effect" = deffect,
      "Reference" = dref,
      "ICER" = icer
    ) %>%
    select(-hsumm, esumm)
  
  trace_res_xlsx <- trace_res %>%
    rename(
      "Day" = model_day,
      "Week" = model_week,
      "Month" = model_month,
      "Year" = model_year,
      "Strategy" = series
    )
  cor_trace_res_xlsx <- cor_trace_res %>%
    rename(
      "Day" = model_day,
      "Week" = model_week,
      "Month" = model_month,
      "Year" = model_year,
      "Strategy" = series
    )
  
  param_res <- compile_parameters(res)
  trans_res <- compile_transitions(res)
  
  if (nrow(trans_res) == 0) {
    trans_res <- trace_res
  }
  
  values_res <- compile_values(res)
  unit_values_res <- compile_unit_values(res)
  
  if(length(dots$tables) > 0) {
    tables_list <- dots$tables
    names(tables_list) <- paste0("Tbl - ", names(dots$tables))
  } else {
    tables_list <- list()
  }
  
  wb_list <- list(
    "Inputs - Settings" = data.frame(setting = names(dots$settings), value = as.character(dots$settings)),
    "Inputs - Groups" = dots$groups,
    "Inputs - Strategies" = dots$strategies,
    "Inputs - States" = dots$states,
    "Inputs - Transitions" = dots$transitions,
    "Inputs - Health Values" = dots$hvalues,
    "Inputs - Econ Values" = dots$evalues,
    "Inputs - Health Summ" = dots$hsumms,
    "Inputs - Econ Summ" = dots$esumms,
    "Inputs - Parameters" = dots$variables,
    "Inputs - Surv Dists" = dots$surv_dists
  ) %>%
    append(tables_list) %>%
    append(list(
      "Calc - Params"= param_res,
      "Calc - Trans"= trans_res,
      "Calc - Unit Values"= unit_values_res,
      "Calc - Values"= values_res,
      "Results - Trace" = trace_res,
      "Results - Trace (Corrected)" = cor_trace_res,
      "Results - Outcomes" = outcomes_res,
      "Results - Costs" = costs_res,
      "Results - CE" = ce_res,
      "Results - NMB" = nmb_res
    )) %>%
    purrr::keep(function(x) {
      isNull <- is.null(x)
      dimensions <- c(nrow(x), ncol(x))
      !isNull && !all(is.na(dimensions)) && all(dimensions) > 0
    })
  dots$report_progress(1L)
  filename <- paste0(dots$name, ".xlsx")
  write_workbook(lapply(wb_list, sanitize_df), filename)
  if (!is.null(dots$.manifest)) {
    dots$.manifest$register_file('excel_output', filename, 'Export to excel output', default = T)
  }
  ret <- wb_list
  
}

compile_parameters <- function(x) {
  if (is.null(x$demographics)) {
    # Homogenous model
    lapply(x$model_runs$eval_strategy_list, function(x) x$parameters) %>%
      do.call(rbind, .) %>%
      as_tibble()
  } else {
    # Heterogeneous model
    lapply(x$demographics$model_list, function(x) {
      lapply(x$.mod, function(x) x$parameters)
    }) %>%
      unlist(recursive=F) %>%
      do.call(rbind, .) %>%
      as_tibble()
  }
}

compile_parameters <- function(x) {
  lapply(x$.mod, function(x) x$parameters) %>%
    do.call(rbind, .) %>%
    as_tibble() %>%
    select_if(names(.) != '.group_weight')
}

compile_transitions <- function(x) {
  the_class <-  class(x$.mod[[1]]$transition)
  if("eval_part_surv" %in% the_class) {
    compile_transitions_psm(x)
  } else {
    if("eval_part_surv_custom" %in% the_class) {
      compile_transitions_custom(x)
    } else {
      compile_transitions_markov(x)
    }
  }
}

compile_transitions_psm <- function(x) {
  x %>%
    rowwise() %>%
    group_split() %>%
    map(function(model_run) {
      res <- model_run$.mod[[1]]
      data.frame(
        cycle = seq_len(length(res$transition$pfs_surv)) - 1,
        pfs = res$transition$pfs_surv,
        os = res$transition$os_surv,
        series = model_run$series
      ) %>%
        mutate(group = suppressWarnings(model_run$.group_scen))
    }) %>%
    bind_rows() %>%
    select_if(names(.) %in% c("strategy", "group", "cycle", "pfs", "os")) %>%
    as_tibble()
}

compile_transitions_markov <- function(x) {
  state_names <- rownames(x$.mod[[1]]$transition[[1]])
  n_states <- length(state_names)
  n_cycles <- length(x$.mod[[1]]$transition)
  x %>%
    rowwise() %>%
    group_split() %>%
    map(function(model_run) {
      model_run$.mod[[1]]$transition %>%
        map2(seq_len(length(.)), function(mat, i) {
          as.matrix(mat) %>%
            as.data.frame(stringsAsFactors = F) %>%
            mutate(
              from = state_names,
              cycle = i,
              strategy = model_run$series,
              group = suppressWarnings(model_run$.group_scen)
            )
        }) %>%
        bind_rows()
    }) %>%
    bind_rows() %>%
    select(intersect(c("strategy", "group", "cycle", "from", state_names), names(.))) %>%
    as_tibble()
}

compile_transitions_custom <- function(x) {
  data.frame()
}


compile_unit_values <- function(x) {
  
  x %>%
    rowwise() %>%
    group_split() %>%
    map(function(row) {
      map2(
        row$.mod[[1]]$states,
        names(row$.mod[[1]]$states),
        function(unit_values, name) mutate(unit_values, state = name)
      ) %>%
        bind_rows() %>%
        mutate(group = suppressWarnings(row$.group_scen), strategy = row$series)
    }) %>%
    bind_rows() %>%
    rename(cycle = markov_cycle) %>%
    select(intersect(c('strategy', 'group', 'state', names(.)), names(.))) %>%
    arrange(!!!syms(intersect(c('strategy', 'group', 'state', 'cycle'), names(.))))
  
}

compile_values <- function(x) {
  
  x %>%
    rowwise() %>%
    group_split() %>%
    map(function(row) {
      mutate(row$.mod[[1]]$values, group = suppressWarnings(row$.group_scen), strategy = row$series)
    }) %>%
    bind_rows() %>%
    rename(cycle = markov_cycle) %>%
    select(intersect(c('strategy', 'group', names(.)), names(.))) %>%
    arrange(!!!syms(intersect(c('strategy', 'group', 'cycle'), names(.))))
}



write_workbook <- function(dflist, path, ...){
  sheet_names <- strtrim(names(dflist), 31)
  wb <- createWorkbook()
  for(i in 1:length(dflist)){
    addWorksheet(wb, sheet_names[i])
    writeDataTable(wb, sheet_names[i], dflist[[i]],...)
    setColWidths(wb, sheet_names[i], cols = seq_len(ncol(dflist[[i]])), widths = 24)
    freezePane(wb, sheet_names[i], firstActiveRow = 2, firstActiveCol = 1)
  }
  saveWorkbook(wb, path, overwrite = TRUE)
}

read_workbook <- function(path, ...) {
  sheet_names <- getSheetNames(path)
  df_list <- sapply(
    sheet_names,
    function(name) as_tibble(openxlsx::read.xlsx(path, sheet = name, ...)),
    USE.NAMES = TRUE
  )
  return(df_list)
}