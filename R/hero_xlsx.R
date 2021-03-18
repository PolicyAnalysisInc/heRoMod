#' @export
export_hero_xlsx <- function(...) {
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
  trace_res <- extract_sa_bc_trace(res, corrected = F)
  cor_trace_res <- extract_sa_bc_trace(res, corrected = T)
  outcome_res <- extract_sa_bc_summaries(res, dots$hsumms)
  costs_res <- extract_sa_bc_summaries(res, dots$esumms)
  ce_res <- extract_sa_bc_incrmental_ce(outcome_res, costs_res)
  pw_ce_res <- extract_sa_bc_pairwise_ce(outcome_res, costs_res)
  nmb_res <- extract_sa_bc_nmb(outcome_res, costs_res, dots$hsumms)
    
  
  outcome_res_xlsx <- outcome_res %>%
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
  
  param_res <- compile_parameters_(heemod_res)
  trans_res <- compile_transitions_(heemod_res)
  
  if (nrow(trans_res) == 0) {
    trans_res <- trace_res
  }
  
  unit_values_res <- compile_unit_values(heemod_res)
  values_res <- compile_values(heemod_res)
  
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
      "Results - Outcomes" = health_res,
      "Results - Costs" = econ_res,
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
  writeWorkbook(lapply(wb_list, sanitize_df), filename)
  if (!is.null(dots$.manifest)) {
    dots$.manifest$register_file('excel_output', filename, 'Export to excel output', default = T)
  }
  ret <- wb_list
  
}

#' @export
export_hero_xlsx_ <- function(...) {
  # Capture arguments
  dots <- patch_progress_funcs(list(...))
  
  # Compile model object
  args <- do.call(build_hero_model, dots)
  args$run_demo = !(is.null(args$demo))
  
  max_prog <- get_excel_max_progress(dots)
  try(dots$report_max_progress(max_prog))
  
  # Run model
  heemod_res <- do.call(run_model_api, args)
  
  # Extract Main Results
  if(is.null(heemod_res$demographics)) {
    main_res <- heemod_res$model_runs
  } else {
    main_res <- heemod_res$demographics$combined_model
  }
  
  health_res <- hero_extract_summ(main_res, dots$hsumms)
  econ_res <- hero_extract_summ(main_res, dots$esumms)
  nmb_res <- hero_extract_nmb(health_res, econ_res, dots$hsumms) %>%
    rename(
      "Outcome" = outcome,
      "Strategy" = series,
      "Component" = group,
      "Type" = type,
      "Value" = value
    ) %>%
    select(-disc)
  health_res <- health_res %>%
    rename(
      "Outcome" = outcome,
      "Strategy" = series,
      "Component" = group,
      "Discounted" = disc,
      "Value" = value
    )
  econ_res <- econ_res %>%
    rename(
      "Outcome" = outcome,
      "Strategy" = series,
      "Component" = group,
      "Discounted" = disc,
      "Value" = value
    )
  ce_res <- hero_extract_ce(main_res, dots$hsumms, dots$esumms) %>%
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
  trace_res <- hero_extract_trace(main_res) %>%
    rename(
      "Day" = model_day,
      "Week" = model_week,
      "Month" = model_month,
      "Year" = model_year,
      "Strategy" = series
    )
  cor_trace_res <- hero_extract_trace(main_res, T) %>%
    rename(
      "Day" = model_day,
      "Week" = model_week,
      "Month" = model_month,
      "Year" = model_year,
      "Strategy" = series
    )
  param_res <- compile_parameters(heemod_res)
  trans_res <- compile_transitions(heemod_res)
  if (nrow(trans_res) == 0) {
    trans_res <- trace_res
  }
  unit_values_res <- compile_unit_values(heemod_res)
  values_res <- compile_values(heemod_res)
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
      "Results - Outcomes" = health_res,
      "Results - Costs" = econ_res,
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

compile_parameters_ <- function(x) {
  lapply(x$.mod, function(x) x$parameters) %>%
    do.call(rbind, .) %>%
    as_tibble() %>%
    select_if(names(.) != '.group_weight')
}

compile_transitions <- function(x) {
  if (is.null(x$demographics)) {
    # Homogenous model
    the_class <-  class(x$model_runs$eval_strategy_list[[1]]$transition)
    if("eval_part_surv" %in% the_class) {
      plyr::ldply(x$model_runs$eval_strategy_list, function(y) {
        data.frame(
          cycle = seq_len(length(y$transition$pfs_surv)) - 1,
          pfs = y$transition$pfs_surv,
          os = y$transition$os_surv
        )
      }, .id = "strategy") %>%
        select(!!!syms(c("strategy", "cycle", "pfs", "os"))) %>%
        as_tibble()
    } else {
      if("eval_part_surv_custom" %in% the_class) {
        data.frame()
      } else {
        state_names <- rownames(x$model_runs$eval_strategy_list[[1]]$transition[[1]])
        n_states <- length(state_names)
        n_cycles <- length(x$model_runs$eval_strategy_list[[1]]$transition)
        plyr::ldply(x$model_runs$eval_strategy_list, function(y) {
          do.call(rbind, y$transition) %>%
            as.data.frame(stringsAsFactors=F) %>%
            mutate(
              from = rep(state_names, n_cycles),
              cycle = rep(seq_len(n_cycles), each = n_states)
            )
        }, .id = "strategy") %>%
          select(!!!syms(c("strategy", "cycle", "from", state_names))) %>%
          as_tibble()
      }
    }
  } else {
    # Heterogeneous model
    the_class <- class(x$demographics$model_list[[1]]$.mod[[1]]$transition)
    if("eval_part_surv" %in% the_class) {
      state_names <- rownames(x$demographics$model_list[[1]]$.mod[[1]]$transition[[1]])
      n_states <- length(state_names)
      n_cycles <- length(x$demographics$model_list[[1]]$.mod[[1]]$transition)
      plyr::ldply(x$demographics$model_list, function(x) {
        group_names <- as.character(lapply(x$.mod, function(x) x$parameters$.group[1]))
        group_list <- x$.mod
        names(group_list) <- group_names
        plyr::ldply(group_list, function(y) {
          data.frame(
            cycle = seq_len(length(y$transition$pfs_surv)) - 1,
            pfs = y$transition$pfs_surv,
            os = y$transition$os_surv
          )
        }, .id = "group")
      }, .id = "strategy") %>%
        select(!!!syms(c("strategy", "group", "cycle", "pfs", "os"))) %>%
        as_tibble()
      
    } else {
      if ("eval_part_surv_custom" %in% the_class) {
        data.frame()
      } else {
        state_names <- rownames(x$demographics$model_list[[1]]$.mod[[1]]$transition[[1]])
        n_states <- length(state_names)
        n_cycles <- length(x$demographics$model_list[[1]]$.mod[[1]]$transition)
        plyr::ldply(x$demographics$model_list, function(x) {
          group_names <- as.character(lapply(x$.mod, function(x) x$parameters$.group[1]))
          group_list <- x$.mod
          names(group_list) <- group_names
          plyr::ldply(group_list, function(y) {
            do.call(rbind, y$transition) %>%
              as.data.frame(stringsAsFactors=F) %>%
              mutate(
                from = rep(state_names, n_cycles),
                cycle = rep(seq_len(n_cycles), each = n_states)
              )
          }, .id = "group")
        }, .id = "strategy") %>%
          select(!!!syms(c("strategy", "group", "cycle", "from", state_names))) %>%
          as_tibble()
      }
    }
  }
  
}

compile_transitions_ <- function(x) {
  if (is.null(x$demographics)) {
    # Homogenous model
    the_class <-  class(x$model_runs$eval_strategy_list[[1]]$transition)
    if("eval_part_surv" %in% the_class) {
      plyr::ldply(x$model_runs$eval_strategy_list, function(y) {
        data.frame(
          cycle = seq_len(length(y$transition$pfs_surv)) - 1,
          pfs = y$transition$pfs_surv,
          os = y$transition$os_surv
        )
      }, .id = "strategy") %>%
        select(!!!syms(c("strategy", "cycle", "pfs", "os"))) %>%
        as_tibble()
    } else {
      if("eval_part_surv_custom" %in% the_class) {
        data.frame()
      } else {
        state_names <- rownames(x$model_runs$eval_strategy_list[[1]]$transition[[1]])
        n_states <- length(state_names)
        n_cycles <- length(x$model_runs$eval_strategy_list[[1]]$transition)
        plyr::ldply(x$model_runs$eval_strategy_list, function(y) {
          do.call(rbind, map(y$transition, as.matrix)) %>%
            as.data.frame(stringsAsFactors=F) %>%
            mutate(
              from = rep(state_names, n_cycles),
              cycle = rep(seq_len(n_cycles), each = n_states)
            )
        }, .id = "strategy") %>%
          select(!!!syms(c("strategy", "cycle", "from", state_names))) %>%
          as_tibble()
      }
    }
  } else {
    # Heterogeneous model
    the_class <- class(x$demographics$model_list[[1]]$.mod[[1]]$transition)
    if("eval_part_surv" %in% the_class) {
      state_names <- rownames(x$demographics$model_list[[1]]$.mod[[1]]$transition[[1]])
      n_states <- length(state_names)
      n_cycles <- length(x$demographics$model_list[[1]]$.mod[[1]]$transition)
      plyr::ldply(x$demographics$model_list, function(x) {
        group_names <- as.character(lapply(x$.mod, function(x) x$parameters$.group[1]))
        group_list <- x$.mod
        names(group_list) <- group_names
        plyr::ldply(group_list, function(y) {
          data.frame(
            cycle = seq_len(length(y$transition$pfs_surv)) - 1,
            pfs = y$transition$pfs_surv,
            os = y$transition$os_surv
          )
        }, .id = "group")
      }, .id = "strategy") %>%
        select(!!!syms(c("strategy", "group", "cycle", "pfs", "os"))) %>%
        as_tibble()
      
    } else {
      if ("eval_part_surv_custom" %in% the_class) {
        data.frame()
      } else {
        state_names <- rownames(x$demographics$model_list[[1]]$.mod[[1]]$transition[[1]])
        n_states <- length(state_names)
        n_cycles <- length(x$demographics$model_list[[1]]$.mod[[1]]$transition)
        plyr::ldply(x$demographics$model_list, function(x) {
          group_names <- as.character(lapply(x$.mod, function(x) x$parameters$.group[1]))
          group_list <- x$.mod
          names(group_list) <- group_names
          plyr::ldply(group_list, function(y) {
            do.call(rbind, map(y$transition, as.matrix)) %>%
              as.data.frame(stringsAsFactors=F) %>%
              mutate(
                from = rep(state_names, n_cycles),
                cycle = rep(seq_len(n_cycles), each = n_states)
              )
          }, .id = "group")
        }, .id = "strategy") %>%
          select(!!!syms(c("strategy", "group", "cycle", "from", state_names))) %>%
          as_tibble()
      }
    }
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