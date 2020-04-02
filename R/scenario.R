run_hero_scen_ <- function(...) {
  
  # Capture arguments
  dots <- list(...)
  
  if(nrow(as.data.frame(dots$groups)) <= 1) {
    # Homogenous model
    # Compile model object
    args <- do.call(build_hero_model, dots)
    args$run_scen = TRUE
    
    # Run Model
    heemod_res <- do.call(run_model_api, args)
    
    # Extract BC results
    
    bc_outcome_res <- hero_extract_summ(heemod_res$model_runs, dots$hsumms)
    bc_cost_res <- hero_extract_summ(heemod_res$model_runs, dots$esumms)
    bc_nmb_res <- hero_extract_nmb(bc_outcome_res, bc_cost_res, dots$hsumms)
    # bc_ce_res <- hero_extract_ce(heemod_res$model_runs, dots$hsumms, dots$esumms)
    # 
    # # Extract DSA results
    outcome_res <- hero_extract_scen_summ(heemod_res$scen, bc_outcome_res, dots$hsumms)
    cost_res <- hero_extract_scen_summ(heemod_res$scen, bc_cost_res, dots$esumms)
    # ce_res <- hero_extract_dsa_ce(heemod_res$dsa, dots$hsumms, dots$esumms)
    nmb_res <- hero_extract_scen_nmb(outcome_res, cost_res, bc_nmb_res, dots$hsumms, dots$esumms)

    ret <- list(
      outcomes = outcome_res,
      cost = cost_res,
      nmb = nmb_res
    )
    
  } else {
    # Heterogeneous model
    # Run scenario analysis for each group
    scens <- plyr::alply(dots$groups, 1, function(x) {
      
      # Run model for given group
      group_args <- dots
      group_args$groups <- x
      group_model <- do.call(run_hero_scen_, group_args)
      # 
      # # Extract results and apply weights
      group_model$outcomes$value <- group_model$outcomes$value * as.numeric(x$weight)
      group_model$cost$value <- group_model$cost$value * as.numeric(x$weight)
      group_model$nmb$value <- group_model$nmb$value * as.numeric(x$weight)
      group_model
    })
    
    # Aggregate results over all groups
    
    weights <- as.numeric(dots$groups$weight)
    nmb_res <- scens[[1]]$nmb
    nmb_res$value <- Reduce(`+`, purrr::map(scens, ~ .$nmb$value)) / sum(weights)

    outcomes_res <- scens[[1]]$outcomes
    outcomes_res$value <- Reduce(`+`, purrr::map(scens, ~ .$outcomes$value)) / sum(weights)

    cost_res <- scens[[1]]$cost
    cost_res$value <- Reduce(`+`, purrr::map(scens, ~ .$cost$value)) / sum(weights)

    ret <- list(
      outcomes = outcomes_res,
      cost = cost_res,
      nmb = nmb_res
    )
    
  }
  ret
}

#' @export
run_hero_scen <- function(...) {
  dots <- list(...)
  if (!'data.frame' %in% class(dots$scenario)) {
    stop('Cannot run scenario analysis: no scenarios were defined.', call. = F)
  }
  scen_param_count <- dots$scenario %>%
    group_by(scenario_name, param_name) %>%
    summarize(n=n())
  dupe <- scen_param_count$n > 1
  scenario_descs <- dots$scenario %>%
    group_by(scenario_name, description) %>%
    summarise()
  if (any(dupe)) {
    index <- which(dupe)
    dupe_scen <- scen_param_count[index[1], ]$scenario_name
    dupe_param <- scen_param_count[index[1], ]$param_name
    stop(
      paste0(
        'Error in scenario "', dupe_scen, '", parameter "',
        dupe_param, '" is used more than once.'
      ),
      call. = F
    )
  }
  # Run the DSA
  res <- run_hero_scen_(...)
  # Compress the results
  res$nmb <- res$nmb  %>%
    left_join(
      scenario_descs,
      by = c("scenario" = "scenario_name")
    ) %>%
    ungroup() %>%
    mutate(
      scenario = factor(scenario, levels = unique(c("Base Case", dots$scenario$scenario_name))),
      description = ifelse(
        scenario == "Base Case",
        "Base case scenario of model.",
        description
      )
    ) %>%
    group_by(health_outcome, econ_outcome, series) %>%
    group_split() %>%
    purrr::map(function(x) {
      list(
        health_outcome = x$health_outcome[1],
        econ_outcome = x$econ_outcome[1],
        series = x$series[1],
        data = arrange(
          select(x, -health_outcome, -econ_outcome, -series),
          scenario
        )
      )
    }) 
  res$cost <- res$cost %>%
    left_join(
      scenario_descs,
      by = c("scenario" = "scenario_name")
    ) %>%
    ungroup() %>%
    mutate(
      scenario = factor(scenario, levels = unique(c("Base Case", dots$scenario$scenario_name))),
      description = ifelse(
        scenario == "Base Case",
        "Base case scenario of model.",
        description
      )
    ) %>%
    group_by(outcome, disc, series) %>%
    group_split() %>%
    purrr::map(function(x) {
      list(
        outcome = x$outcome[1],
        disc = x$disc[1],
        series = x$series[1],
        data = arrange(
          select(x, -outcome, -disc, -series),
          scenario
        )
      )
    }) 
  res$outcomes <- res$outcomes %>%
    left_join(
      scenario_descs,
      by = c("scenario" = "scenario_name")
    ) %>%
    ungroup() %>%
    mutate(
      scenario = factor(scenario, levels = unique(c("Base Case", dots$scenario$scenario_name))),
      description = ifelse(
        scenario == "Base Case",
        "Base case scenario of model.",
        description
      )
    ) %>%
    group_by(outcome, disc, series) %>%
    group_split() %>%
    purrr::map(function(x) {
      list(
        outcome = x$outcome[1],
        disc = x$disc[1],
        series = x$series[1],
        data = arrange(
          select(x, -outcome, -disc, -series),
          scenario
        )
      )
    }) 
  
  res$api_ver <- '2.0'
  
  res
}

run_scen <- function(model, scen, cores = 1, report_progress = NULL) {
  # 
  if (! all(c(".cost", ".effect") %in% names(get_model_results(model)))) {
    stop("No cost and/or effect defined, sensitivity analysis unavailable.")
  }

  init <- get_uneval_init(model)
  cycles <- get_cycles(model)
  method <- get_method(model)
  strategy_names <- get_strategy_names(model)
  scenario_names <- unique(scen$scenario_name)
  par_names <- unique(scen$param_name)
  
  new_data <- scen %>%
    mutate(scenario_name = factor(scenario_name, levels = unique(scenario_name))) %>%
    group_by(scenario_name) %>%
    group_split() %>%
    lapply(function(x) {
      args_list <- lapply(x$formula, list)
      names(args_list) <- x$param_name
      params_in_scen <- x$param_name
      params_not <- setdiff(par_names, params_in_scen)
      params_not_list <- rep(NA, length(params_not))
      names(params_not_list) <- params_not
      do.call(tibble::tibble, c(args_list, params_not_list))
    }) %>%
    do.call(rbind, .)
 
  list_res <- list()
  e_newdata <- list()
  for (n in strategy_names) {
    message(sprintf(
      "Running scenarios for strategy '%s'...", n
    ))
    n_scenario <- nrow(new_data)
    n_var <- ncol(new_data)
    var_names <- colnames(new_data)
    bc_param <- model$eval_strategy_list[[n]]$parameters
    for (i in seq_len(n_var)) {
      new_data[[i]] <- lapply(seq_len(n_scenario), function(j) {
        lazy_param <- new_data[[i]][[j]]
        if ("lazy" %in% class(new_data[[i]][[j]])) {
          lazy_param$env <- new.env(parent = new_data[[i]][[j]]$env)
          lazy_param$env$bc <- bc_param[[var_names[i]]]
        }
        lazy_param
      })
    }
    tab <- eval_strategy_newdata(
      model,
      strategy = n,
      newdata = new_data,
      cores = cores,
      report_progress = report_progress
    )

    res <- tab %>%
      mutate_if(
        names(tab) %in% par_names,
        list(to_text_dots),
        name = FALSE
      )

    list_res <- c(
      list_res,
      list(res)
    )

  }

  for (i in seq_along(strategy_names)) {
    list_res[[i]]$.strategy_names <- strategy_names[i]
    list_res[[i]]$.scenario <- scenario_names
  }

  res <- bind_rows(list_res)
  res <- res %>%
    rowwise() %>%
    do({get_total_state_values(.$.mod)}) %>%
    bind_cols(select(res, !!quo(- .mod))) %>% 
    ungroup() %>%
    mutate(!!!lazyeval::lazy_eval(get_ce(model), data = .))

  structure(
    list(
      scen = res,
      model = model
    ),
    class = c("scenario_analysis", "list")
  )
}

get_model.scen <- function(x) {
  x$model
}


hero_extract_scen_summ <- function(res, bc_res, summ) {
  
  bc_res_summs <- bc_res %>%
    group_by(outcome,series, disc) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  
  bc_res_all <- plyr::rbind.fill(
    bc_res %>% mutate(outcome=group) %>% distinct(outcome, series, group, disc, .keep_all = T),
    bc_res_summs
  ) %>%
    transmute(
      outcome,
      disc,
      scenario = "Base Case",
      series,
      value
    ) %>%
  filter(!grepl(" vs. ", series, fixed = T))
  
  value_res <- as.data.frame(res$scen, stringsAsFactors=F)
  
  strategies <- unique(value_res$.strategy_names)
  n_strat <- length(strategies)
  value_names <- c(
    c('.strategy_names', '.scenario'),
    colnames(value_res)[seq_len(which(colnames(value_res)==".n_indiv") - 1)]
  )
  
  all_res <- value_res %>%
    .[ ,value_names] %>%
    reshape2::melt(id.vars = c(".strategy_names", ".scenario")) %>%
    mutate(variable = as.character(variable))
  
  summ_unique <- tibble::tibble(outcome = unique(c(summ$value, summ$name)))
  
  undisc <- inner_join(
    rename(summ_unique,variable = outcome),
    all_res,
    by = "variable"
  ) %>%
    mutate(
      scenario = .scenario,
      outcome = variable,
      series = .strategy_names,
      disc = F
    ) %>%
    select(outcome, disc, scenario, series, value)
  
  disc <- inner_join(
    mutate(
      summ_unique,
      variable1 = paste0(".disc_", outcome),
      variable = outcome
    ) %>%
      select(-outcome),
    all_res,
    by = c("variable1" = "variable")
  ) %>%
    mutate(
      scenario = .scenario,
      outcome = variable,
      series = .strategy_names,
      group = variable,
      disc = T
    ) %>%
    select(outcome, disc, scenario, series, value)
  rbind(bc_res_all, disc, undisc) %>%
    arrange(outcome, disc, scenario, series)
}

hero_extract_scen_nmb <- function(hsumm_res, esumm_res, bc_res, hsumms, esumms) {
  
  distinct_hsumms <- distinct(hsumms, name, wtp)
  distinct_esumms <- distinct(esumms, name)
  
  # Get NMBs for each DSA scenario
  h_nmb <- hsumm_res %>%
    filter(disc) %>%
    inner_join(distinct_hsumms, by = c("outcome" = "name")) %>%
    transmute(
      scenario,
      outcome,
      series,
      value = value * wtp
    )
  
  e_nmb <- esumm_res %>%
    filter(disc) %>%
    inner_join(distinct_esumms, by = c("outcome" = "name")) %>%
    transmute(
      scenario,
      outcome,
      series,
      value = -value
    )
  
  expand.grid(
    health_outcome = unique(hsumms$name),
    econ_outcome = unique(esumms$name),
    stringsAsFactors = F
  ) %>%
    plyr::ddply(c("health_outcome", "econ_outcome"), function(x) {
      rbind(
        filter(h_nmb, outcome == x$health_outcome),
        filter(e_nmb, outcome == x$econ_outcome)
      )
    }) %>%
    group_by(scenario, series, health_outcome, econ_outcome) %>%
    summarise(value = sum(value)) %>%
    arrange(health_outcome, econ_outcome, scenario, series)
  
}
