
parse_hero_vars <- function(data, clength, hdisc, edisc, groups) {
  hdisc_adj <- rescale_discount_rate(hdisc, 365, clength)
  edisc_adj <- rescale_discount_rate(edisc, 365, clength)
  hero_pars <- tibble::tribble(
    ~parameter,            ~value,
    "cycle_length_days",   as.character(clength),
    "cycle_length_weeks",  "cycle_length_days / 7",
    "cycle_length_months", "cycle_length_days * 12 / 365",
    "cycle_length_years",  "cycle_length_days / 365",
    "model_day",           "markov_cycle * cycle_length_days",
    "model_week",          "markov_cycle * cycle_length_weeks",
    "model_month",         "markov_cycle * cycle_length_months",
    "model_year",          "markov_cycle * cycle_length_years",
    "state_day",           "state_time * cycle_length_days",
    "state_week",          "state_time * cycle_length_weeks",
    "state_month",         "state_time * cycle_length_months",
    "state_year",          "state_time * cycle_length_years",
    "disc_h",              paste0("discount(1, ", hdisc_adj, ")"),
    "disc_e",              paste0("discount(1, ", edisc_adj, ")")
  )
  if((class(groups) %in% "data.frame") && (nrow(groups) > 0)) {
    groups <- groups %>%
      dplyr::rename_(.dots = c(".group" = "name"))
    group_vars <- groups %>%
      colnames() %>%
      setdiff(".weights") %>%
      plyr::ldply(function(name) data.frame(parameter = name, value = groups[[name]][1]))
  } else {
    group_vars <- NULL
  }
  if((class(data) %in% "data.frame") && (nrow(data) > 0)) {
    user_pars <- dplyr::transmute(
      data,
      parameter = name,
      value = value
    )
  } else {
    user_pars <- NULL
  }
  rbind(
    hero_pars,
    group_vars,
    user_pars
  )
}
parse_hero_obj_vars <- function(data) {
  if((class(data) %in% "data.frame") && (nrow(data) > 0)) {
    dplyr::transmute(
      data,
      parameter = name,
      value = value
    )
  } else {
    NULL
  }
}
parse_hero_groups <- function(data) {
  if((class(data) %in% "data.frame") && (nrow(data) > 1)) {
    dplyr::rename_(data, .dots = c(".group" = "name", ".weights" = "weight")) %>%
    dplyr::mutate(.weights = as.numeric(.weights))
  } else {
    NULL
  }
}
parse_hero_values <- function(data, health, strategies, states, clength) {
  
  trans_string <- "→"
  
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
  state_vals <- data %>%
    dplyr::filter(!grepl(trans_string, state, fixed = T))
  
  states_undisc <- state_vals %>%
    dplyr::rowwise() %>%
    dplyr::do({
      if(.$strategy == "All") {
        if(.$state == "All") {
          data.frame(
            name = .$name,
            .model = rep(strategies, length(states)),
            .state = rep(states, each = length(strategies)),
            value = .$value,
            stringsAsFactors = F
          )
        } else {
          data.frame(
            name = .$name,
            .model = strategies,
            .state = .$state,
            value = .$value,
            stringsAsFactors = F
          )
        }
      } else {
        if(.$state == "All") {
          data.frame(
            name = .$name,
            .model = .$strategy,
            .state = states,
            value = .$value,
            stringsAsFactors = F
          )
        } else {
          data.frame(
            name = .$name,
            .model = .$strategy,
            .state = .$state,
            value = .$value,
            stringsAsFactors = F
          )
        }
      }
    }) %>%
    dplyr::ungroup()
  
  if(nrow(states_undisc) > 0) {
    states_disc <- states_undisc %>%
      dplyr::mutate(
        value = disc_fun(name),
        name = paste0(".disc_", name)
      )
  } else {
    states_disc <- NULLs
  }
  rbind(states_undisc, states_disc)
}
parse_hero_values_st <- function(data, health, strategies, clength) {
  
  trans_string <- "→"
  
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
  state_trans <- data %>%
    dplyr::filter(grepl(trans_string, state, fixed = T))
  
  states_undisc <- state_trans %>%
    dplyr::rowwise() %>%
    dplyr::do({
      if(.$strategy == "All") {
        data.frame(
          name = .$name,
          .model = strategies,
          .transition = .$state,
          value = .$value,
          stringsAsFactors = F
        )
      } else {
        data.frame(
          name = .$name,
          .model = .$strategy,
          .transition = .$state,
          value = .$value,
          stringsAsFactors = F
        )
      }
    }) %>%
    dplyr::ungroup()

  if (nrow(states_undisc) > 0) {
    states_disc <- states_undisc %>%
      dplyr::mutate(
        value = disc_fun(name),
        name = paste0(".disc_", name)
      )
  } else {
    states_disc <- NULL
  }
  
  rbind(states_undisc, states_disc)
}
parse_hero_summaries <- function(data, values, health, strategies, states, clength) {
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
  state_summs <- dplyr::filter(data, value %in% values$name)
  
  sum_undisc <- plyr::ddply(state_summs, "name", function(x) {
    data.frame(
      .model = rep(strategies, length(states)),
      .state = rep(states, each = length(strategies)),
      value = paste(x$value, collapse="+"),
      stringsAsFactors = F
    )
  })
  
  if(nrow(sum_undisc) > 0) {
    sum_disc <- sum_undisc %>%
      dplyr::mutate(
        value = disc_fun(name),
        name = paste0(".disc_", name)
      )
  } else {
    sum_disc <- NULL
  }
  
  rbind(sum_undisc, sum_disc)
}
parse_hero_summaries_st <- function(data, values, health, strategies, states, clength) {
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
  st_summs <- dplyr::filter(data, value %in% values$name)
  
  sum_undisc <- plyr::ddply(st_summs, "name", function(x) {
    the_values <- dplyr::filter(
      values,
      name %in% x$value
    )
    the_transitions <- unique(the_values$.transition)
    if(nrow(the_values) > 0) {
      data.frame(
        .model = rep(strategies, length(the_transitions)),
        .transition = rep(the_transitions, each = length(strategies)),
        value = rep(paste(x$value, collapse="+"), n = length(the_transitions), each = length(strategies)),
        stringsAsFactors = F
      )
    } else {
      NULL
    }
  })
  
  if(nrow(sum_undisc) > 0) {
    sum_disc <- sum_undisc %>%
      dplyr::mutate(
        value = disc_fun(name),
        name = paste0(".disc_", name)
      )
  } else {
    sum_disc <- NULL
  }
  rbind(sum_undisc, sum_disc)
}
parse_hero_trans <- function(data, strategies) {
  if (!is.null(data$from)) {
    # Markov
    data %>%
      dplyr::rowwise() %>%
      dplyr::do({
        if(.$strategy == "All") {
          data.frame(
            .model = strategies,
            from = .$from,
            to = .$to,
            prob = .$value,
            stringsAsFactors=F
          )
        } else {
          data.frame(
            .model = .$strategy,
            from = .$from,
            to = .$to,
            prob = .$value,
            stringsAsFactors=F
          )
        }
      }) %>%
      dplyr::ungroup()
  } else {
    # PSM
    dplyr::rename(data, .model = strategy)
  }
}
parse_hero_states <- function(hvalues, evalues, hsumms, esumms, strategies, states, clength) {
  all_value_names <- unique(c(
    hvalues$name,
    hsumms$name,
    evalues$name,
    esumms$name
  ))
  all_value_names <- c(
    all_value_names,
    paste0(".disc_", all_value_names)
  )
  values <- rbind(
    parse_hero_values(hvalues, TRUE, strategies, states, clength),
    parse_hero_values(evalues, FALSE, strategies, states, clength)
  )
  summaries <- rbind(
    parse_hero_summaries(hsumms, values, TRUE, strategies, states, clength),
    parse_hero_summaries(esumms, values, FALSE, strategies, states, clength)
  )
  states_df <- rbind(
    values,
    summaries
  ) %>%
    dplyr::mutate(
      .model = factor(.model, levels = strategies),
      .state = factor(.state, levels = states),
      name = factor(name, levels = all_value_names)
    ) %>%
    reshape2::dcast(.model+.state~name, value.var = "value", fill = 0, drop = F)
  
  states_df
}
parse_hero_states_st <- function(hvalues, evalues, hsumms, esumms, strategies, states, clength) {
  all_value_names <- unique(c(
    hvalues$name,
    hsumms$name,
    evalues$name,
    esumms$name
  ))
  all_value_names <- c(
    all_value_names,
    paste0(".disc_", all_value_names)
  )
  trans_string <- "→"
  values <- rbind(
    parse_hero_values_st(hvalues, TRUE, strategies, clength),
    parse_hero_values_st(evalues, FALSE, strategies, clength)
  )
  summaries <- rbind(
    parse_hero_summaries_st(hsumms, values, TRUE, strategies, states, clength),
    parse_hero_summaries_st(esumms, values, FALSE, strategies, states, clength)
  )
  all_values <- rbind(
    values,
    summaries
  )
  if(nrow(all_values) > 0) {
    st_df <- rbind(
      values,
      summaries
    ) %>%
      dplyr::mutate(
        .model = factor(.model, levels = strategies),
        .transition = factor(.transition, levels = unique(values$.transition)),
        name = factor(name, levels = all_value_names)
      ) %>%
      reshape2::dcast(.model+.transition~name, value.var = "value", fill = 0, drop=F)
    split_trans <- strsplit(as.character(st_df$.transition), trans_string)
    st_df <- dplyr::mutate(
      st_df,
      from = purrr::map_chr(split_trans, ~.[1]),
      to = purrr::map_chr(split_trans, ~.[2])
    )
    
  } else {
    st_df <- NULL
  }
  st_df
}
hero_extract_summ <- function(res, summ) {
  
  model_res <- res$run_model
  value_res <- as.data.frame(res$run_model, stringsAsFactors=F)
  
  strategies <- unique(value_res$.strategy_names)
  n_strat <- length(strategies)
  
  indices <- expand.grid(referent = seq_len(n_strat), comparator = seq_len(n_strat)) %>%
    dplyr::filter(referent != comparator)
  value_names <- setdiff(colnames(value_res), ".strategy_names")
  
  ref_res <- value_res[indices$referent, ]
  comp_res <- value_res[indices$comparator, ]
  delta_res <- ref_res
  delta_res[value_names] <- ref_res[value_names] - comp_res[value_names]
  delta_res$.strategy_names <- paste0(ref_res$.strategy_names, " vs. ", comp_res$.strategy_names)
  all_res <- rbind(value_res, delta_res) %>%
    reshape2::melt(id.vars = ".strategy_names") %>%
    dplyr::mutate(variable = as.character(variable))
  
  summ_unique <- dplyr::distinct(summ, name, value)
 
  undisc <- dplyr::inner_join(
    dplyr::rename(summ_unique,variable = value),
    all_res,
    by = "variable"
  ) %>%
    dplyr::mutate(
      outcome = name,
      series = .strategy_names,
      group = variable,
      disc = F
    ) %>%
    dplyr::select(outcome, series, group, disc, value)
  
  disc <- dplyr::inner_join(
    dplyr::mutate(
      summ_unique,
      variable1 = paste0(".disc_", value),
      variable = value
    ) %>%
      dplyr::select(-value),
    all_res,
    by = c("variable1" = "variable")
  ) %>%
    dplyr::mutate(
      outcome = name,
      series = .strategy_names,
      group = variable,
      disc = T
    ) %>%
    dplyr::select(outcome, series, group, disc, value)
  rbind(disc, undisc)
}
hero_extract_nmb <- function(hsumm_res, esumm_res, hsumms) {
  
  unique_hsumms <- dplyr::distinct(hsumms, name, .keep_all = T) %>%
    dplyr::select(name, wtp)
  
  nmb_hsumm_res <- dplyr::filter(hsumm_res, disc, grepl(" vs. ", series, fixed=T)) %>%
    dplyr::left_join(unique_hsumms, by = c("outcome" = "name")) %>%
    dplyr::mutate(nmb = value * as.numeric(wtp), type = "health") %>%
    dplyr::mutate(value = nmb) %>%
    dplyr::select(outcome, series, group, disc, type, value)
  
  nmb_esumm_res <- dplyr::filter(esumm_res, disc, grepl(" vs. ", series, fixed=T)) %>%
    dplyr::mutate(value = -value, type = "economic") %>%
    dplyr::select(outcome, series, group, disc, type, value)
  
  rbind(nmb_hsumm_res, nmb_esumm_res)
}
hero_extract_ce <- function(res, hsumms, esumms) {
  
  unique_hsumms <- paste0(".disc_", unique(hsumms$name))
  unique_esumms <- paste0(".disc_", unique(esumms$name))
  
  expand.grid(
    hsumm = unique_hsumms,
    esumm = unique_esumms,
    stringsAsFactors = F
  ) %>%
    ddply(c("hsumm","esumm"), function(x){
      temp_res <- res
      class(temp_res) <- "list"
      ce <- list(
        .effect = lazyeval::as.lazy(x$hsumm,  res$ce$.effect$env),
        .cost = lazyeval::as.lazy(x$esumm,  res$ce$.cost$env)
      )
      temp_res$run_model <- dplyr::mutate_(temp_res$run_model, .dots = ce)
      class(temp_res) <- c("run_model", "data.frame")
      summary(temp_res)$res_comp %>%
        dplyr::transmute(
          health_outcome = x$hsumm,
          econ_outcome = x$esumm,
          series = .strategy_names,
          cost = .cost,
          eff = .effect,
          dcost = .dcost,
          deffect = .deffect,
          dref = .dref,
          icer = .icer
        )
    })
}
hero_extract_trace <- function(res) {
  if(!is.null(res$oldmodel)) {
    params <- res$oldmodel$eval_strategy_list[[1]]$parameters
  } else {
    params <- res$eval_strategy_list[[1]]$parameters
  }
  
  time <- rbind(
    data.frame(model_day=0,model_week=0,model_month=0,model_year=0),
    dplyr::distinct(
      params,
      model_day,
      model_week,
      model_month,
      model_year
    )
  )
  trace <- ldply(
    res$eval_strategy_list,
    function(x) {
      x$counts_uncorrected
    }
  ) %>%
    dplyr::rename(
      series = .id
    )
  cbind(time, trace)
}

#' @export
run_hero_model <- function(decision, settings, groups, strategies, states, transitions,
                           hvalues, evalues, hsumms, esumms, variables,
                           tables, scripts, cost, effect, surv_dists = NULL, type = "base case", vbp = NULL) {
  params <- parse_hero_vars(
    variables,
    settings$cycle_length,
    settings$disc_eff,
    settings$disc_cost,
    groups
  )
  surv <- parse_hero_obj_vars(surv_dists)
  groups_tbl <- parse_hero_groups(groups)
  trans <- parse_hero_trans(transitions, strategies$name)
  state_list <- parse_hero_states(
    hvalues,
    evalues,
    hsumms,
    esumms,
    strategies$name,
    states$name,
    settings$cycle_length
  )
  st_list <- parse_hero_states_st(
    hvalues,
    evalues,
    hsumms,
    esumms,
    strategies$name,
    states$name,
    settings$cycle_length
  )
  tables <- tables
  limits <- as.numeric(states$limit)
  names(limits) <- states$name
  limits <- limits[!is.na(limits) & !(limits == 0)]
  heemod_res <- run_model_api(
    states = state_list,
    st = st_list,
    tm = trans,
    param = params,
    demo = groups_tbl,
    options = tibble::tribble(
      ~option,  ~value,
      "cost",   paste0(".disc_", cost),
      "effect", paste0(".disc_", effect),
      "method", "life-table",
      "cycles", settings$n_cycles,
      "n",      1,
      "init",   paste(states$prob,collapse=", ")
    ),
    data = tables,
    run_dsa = F,
    run_psa = F,
    run_demo = !is.null(groups_tbl),
    state_time_limit = limits,
    source = scripts,
    aux_params = surv
  )
  
  if(is.null(groups_tbl)) {
    main_res <- heemod_res$model_runs
  } else {
    main_res <- heemod_res$demographics$combined_model
  }
  
  if(type == "base case") {
    health_res <- hero_extract_summ(main_res, hsumms)
    econ_res <- hero_extract_summ(main_res, esumms)
    nmb_res <- hero_extract_nmb(health_res, econ_res, hsumms)
    ce_res <- hero_extract_ce(main_res, hsumms, esumms)
    trace_res <- hero_extract_trace(main_res)
    ret <- list(
      trace = trace_res,
      outcomes = health_res,
      costs = econ_res,
      ce = ce_res,
      nmb = nmb_res
    )
  } else if(type == "vbp") {
    vbp_low <-  lazyeval::as.lazy_dots(setNames(list(0), vbp$par_name), environment())
    vbp_med <-  lazyeval::as.lazy_dots(setNames(list(vbp$wtp/2), vbp$par_name), environment())
    vbp_high <-  lazyeval::as.lazy_dots(setNames(list(vbp$wtp), vbp$par_name), environment())
    vbp_settings <- define_vbp_(vbp$par_name, vbp_low, vbp_med, vbp_high)
    vbp_res <- run_vbp(
      model = main_res,
      vbp = vbp_settings,
      strategy_vbp = vbp$strat,
      wtp_thresholds = c(0, 100000)
    )
    
    ret <- list(
      eq = vbp_res$lin_eq
    )
    
  }
  
  ret
}



#' @export
run_markdown <- function(text, data = NULL) {
  eval_env <- new.env(parent = parent.frame())
  if(!is.null(data)) {
    plyr::l_ply(
      seq_len(length(data)),
      function(i) assign(names(data)[i], data[[i]], envir = eval_env)
    )
  }
  writeLines(text, con = 'output.r')
  knitr::spin('output.r', knit = T, envir = eval_env, precious = F, doc = '^##\\s*')
  ls(eval_env)
}

