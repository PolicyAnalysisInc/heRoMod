#' @export
export_hero_xlsx <- function(...) {

  # Build model object
  dots <- patch_progress_funcs(list(...))
  args <- do.call(build_hero_model, dots)
  
  max_rows <- 200000
  if (!is.null(dots$excel_max_rows)) {
    max_rows <- dots$excel_max_rows
  }
  
  max_prog <- get_excel_max_progress(dots)
  try(dots$progress_reporter$report_max_progress(max_prog))
  
  # Initial model run
  try(dots$progress_reporter$report_progress(1L))
  heemod_res <- do.call(run_model_api, args)
  vbp_name <- dots$vbp$par_name
  
  if ((class(dots$groups) %in% "data.frame") && (nrow(dots$groups) > 1)) {
    
    # Generate sensitivity analysis input table
    groups_table <- gen_groups_table(dots$groups)
    vbp_table <- tibble(.vbp_scen = NA, .vbp_price = NA, .vbp_param = list(NA))
    sa_table <- crossing(groups_table, vbp_table)
    n_row <- nrow(sa_table)
    
    # Run sensitivity Analyses
    res <- run_sa(
      heemod_res$model_runs,
      sa_table, c(),
      create_progress_reporter = dots$create_progress_reporter,
      progress_reporter = dots$progress_reporter,
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
  
  param_res <- compile_parameters(res, row_limit = max_rows)
  trans_res <- compile_transitions(res, row_limit = max_rows)
  
  if (nrow(trans_res) == 0) {
    trans_res <- trace_res
  }
  
  values_res <- compile_values(res, row_limit = max_rows)
  unit_values_res <- compile_unit_values(res, row_limit = max_rows)
  
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
  try(dots$progress_reporter$report_progress(1L))
  filename <- paste0(dots$name, ".xlsx")
  write_workbook(lapply(wb_list, sanitize_df), filename)
  if (!is.null(dots$.manifest)) {
    dots$.manifest$register_file('excel_output', filename, 'Export to excel output', default = T)
  }
  ret <- wb_list
  
  list()
}

compile_parameters <- function(x, row_limit = 200000) {
  
  res <- lapply(x$.mod, function(x) x$parameters) %>%
    do.call(rbind, .) %>%
    as_tibble() %>%
    select_if(names(.) != '.group_weight') %>%
    addGroupColumnIfMissing() %>%
    rename(cycle = markov_cycle) %>%
    relocate(strategy, group, state_time, cycle)
  
  n_rows <- nrow(res)
  
  strats <- unique(res$strategy)
  groups <- unique(res$group)
  
  if (n_rows > row_limit) {
    
    df_list <- res %>%
      group_by(strategy, group) %>%
      group_split()
    
    n_row_groups <- length(df_list)
    
    n_rows_per_row_group <- ceiling(row_limit / n_row_groups)
    
    head_rows <- ceiling(n_rows_per_row_group * 0.8)
    tail_rows <- n_rows_per_row_group - head_rows
    
    res <- df_list %>%
      map(function(x) {
        sorted <- arrange(x, cycle)
        rbind(
          head(x, head_rows),
          tail(x, tail_rows)
        )
      }) %>%
      bind_rows() %>%
      relocate(strategy, group, state_time, cycle) %>%
      arrange(
        factor(strategy, levels = strats),
        factor(group, levels = groups),
        state_time,
        cycle
      )
  }
  
  res
}

compile_transitions <- function(x, row_limit = 200000) {
  the_class <-  class(x$.mod[[1]]$transition)
  if("eval_part_surv" %in% the_class) {
    compile_transitions_psm(x, row_limit = row_limit)
  } else {
    if("eval_part_surv_custom" %in% the_class) {
      compile_transitions_custom(x)
    } else {
      compile_transitions_markov(x, row_limit = row_limit)
    }
  }
}

compile_transitions_psm <- function(x, row_limit = 200000) {
  
  res <- x %>%
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
    select_if(names(.) %in% c("series", "group", "cycle", "pfs", "os")) %>%
    as_tibble() %>%
    addGroupColumnIfMissing() %>%
    rename(strategy = series) %>%
    relocate(strategy, group, cycle, pfs, os)
  
  n_rows <- nrow(res)
  
  strats <- unique(res$strategy)
  groups <- unique(res$group)
  
  if (n_rows < row_limit) {
    return(res)
  }
  
  df_list <- res %>%
    group_by(strategy, group) %>%
    group_split()
  
  n_row_groups <- length(df_list)
  
  n_rows_per_row_group <- ceiling(row_limit / n_row_groups)
  
  head_rows <- ceiling(n_rows_per_row_group * 0.8)
  tail_rows <- n_rows_per_row_group - head_rows
  
  res <- df_list %>%
    map(function(x) {
      sorted <- arrange(x, cycle)
      rbind(
        head(x, head_rows),
        tail(x, tail_rows)
      )
    }) %>%
    bind_rows() %>%
    relocate(strategy, group, cycle) %>%
    arrange(
      factor(strategy, levels = strats),
      factor(group, levels = groups),
      cycle
    )
  
  res
  
}

compile_transitions_markov <- function(x, row_limit = 200000) {
  
  state_names <- rownames(x$.mod[[1]]$transition[[1]])
  
  res <- x %>%
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
    as_tibble() %>%
    addGroupColumnIfMissing() %>%
    relocate(strategy, group, cycle, from)
  
  n_rows <- nrow(res)
  
  if (n_rows < row_limit) {
    return(res)
  }
  
  groups <- unique(res$group)
  strats <- unique(res$strategy)
  from_states <- unique(res$from)
  group_cols <- c('')
  

  
  df_list <- res %>%
    group_by(strategy, group) %>%
    group_split()
  
  n_row_groups <- length(df_list)
  
  n_rows_per_row_group <- ceiling(row_limit / n_row_groups)
  
  head_rows <- ceiling(n_rows_per_row_group * 0.8)
  tail_rows <- n_rows_per_row_group - head_rows
  
  res <- df_list %>%
    map(function(x) {
      sorted <- arrange(x, cycle, from)
      rbind(
        head(x, head_rows),
        tail(x, tail_rows)
      )
    }) %>%
    bind_rows() %>%
    relocate(strategy, group, cycle, from) %>%
    arrange(
      factor(strategy, levels = strats),
      factor(group, levels = groups),
      cycle, 
      factor(from, level = from_states)
    )
  
  res
  
}

compile_transitions_custom <- function(x) {
  data.frame()
}


compile_unit_values <- function(x, row_limit = 200000) {
  
  res <- x %>%
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
    arrange(!!!syms(intersect(c('strategy', 'group', 'state', 'cycle'), names(.)))) %>%
    addGroupColumnIfMissing() %>%
    relocate(strategy, group, cycle, state)
  
  n_rows <- nrow(res)
  
  strats <- unique(res$strategy)
  groups <- unique(res$group)
  states <- unique(res$state)
  
  if (n_rows < row_limit) {
    return(res)
  }
  
  df_list <- res %>%
    group_by(strategy, group, state) %>%
    group_split()
  
  n_row_groups <- length(df_list)
  
  n_rows_per_row_group <- ceiling(row_limit / n_row_groups)
  
  head_rows <- ceiling(n_rows_per_row_group * 0.8)
  tail_rows <- n_rows_per_row_group - head_rows
  
  res <- df_list %>%
    map(function(x) {
      sorted <- arrange(x, cycle)
      rbind(
        head(x, head_rows),
        tail(x, tail_rows)
      )
    }) %>%
    bind_rows() %>%
    relocate(strategy, group, cycle, state) %>%
    arrange(
      factor(strategy, levels = strats),
      factor(group, levels = groups),
      cycle, 
      factor(state, level = states)
    )
  
  res

}

compile_values <- function(x, row_limit = 200000) {
 
  res <- x %>%
    rowwise() %>%
    group_split() %>%
    map(function(row) {
      mutate(row$.mod[[1]]$values, group = suppressWarnings(row$.group_scen), strategy = row$series)
    }) %>%
    bind_rows() %>%
    rename(cycle = markov_cycle) %>%
    select(intersect(c('strategy', 'group', names(.)), names(.))) %>%
    arrange(!!!syms(intersect(c('strategy', 'group', 'cycle'), names(.)))) %>%
    addGroupColumnIfMissing() %>%
    relocate(strategy, group, cycle)
  
  n_rows <- nrow(res)
  
  strats <- unique(res$strategy)
  groups <- unique(res$group)
  
  if (n_rows < row_limit) {
    return(res)
  }
  
  df_list <- res %>%
    group_by(strategy, group) %>%
    group_split()
  
  n_row_groups <- length(df_list)
  
  n_rows_per_row_group <- ceiling(row_limit / n_row_groups)
  
  head_rows <- ceiling(n_rows_per_row_group * 0.8)
  tail_rows <- n_rows_per_row_group - head_rows
  
  res <- df_list %>%
    map(function(x) {
      sorted <- arrange(x, cycle)
      rbind(
        head(x, head_rows),
        tail(x, tail_rows)
      )
    }) %>%
    bind_rows() %>%
    relocate(strategy, group, cycle) %>%
    arrange(
      factor(strategy, levels = strats),
      factor(group, levels = groups),
      cycle
    )
  
  res
}

modelHasGroups <- function(res) {
  !suppressWarnings(is.null(res$group))
}

addGroupColumnIfMissing <- function(res) {
  if (!modelHasGroups(res)) {
    mutate(res, group = 'All Patients')
  } else {
    res
  }
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

get_excel_row_limit <- function() {
  getOption("heromod_excel_row_limit", default = 200000)
}