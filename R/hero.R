
parse_hero_vars <- function(data, clength, hdisc, edisc, groups) {
  hdisc_adj <- rescale_discount_rate(hdisc, 365, clength)
  edisc_adj <- rescale_discount_rate(edisc, 365, clength)
  hero_pars <- tibble::tribble(
    ~parameter,            ~value,                                 ~low, ~high, ~psa,
    "cycle_length_days",   as.character(clength),                  NA,   NA, NA,
    "cycle_length_weeks",  "cycle_length_days / 7",                NA,   NA, NA,
    "cycle_length_months", "cycle_length_days * 12 / 365",         NA,   NA, NA,
    "cycle_length_years",  "cycle_length_days / 365",              NA,   NA, NA,
    "model_day",           "markov_cycle * cycle_length_days",     NA,   NA, NA,
    "model_week",          "markov_cycle * cycle_length_weeks",    NA,   NA, NA,
    "model_month",         "markov_cycle * cycle_length_months",   NA,   NA, NA,
    "model_year",          "markov_cycle * cycle_length_years",    NA,   NA, NA,
    "state_day",           "state_time * cycle_length_days",       NA,   NA, NA,
    "state_week",          "state_time * cycle_length_weeks",      NA,   NA, NA,
    "state_month",         "state_time * cycle_length_months",     NA,   NA, NA,
    "state_year",          "state_time * cycle_length_years",      NA,   NA, NA,
    "disc_h",              paste0("discount(1, ", hdisc_adj, ")"), NA,   NA, NA,
    "disc_e",              paste0("discount(1, ", edisc_adj, ")"), NA,   NA, NA
  )
  if((class(groups) %in% "data.frame") && (nrow(groups) > 0)) {
    groups <- groups %>%
      dplyr::mutate(group = name) %>%
      dplyr::rename_(.dots = c(".group" = "name"))
    group_vars <- groups %>%
      colnames() %>%
      setdiff(".weights") %>%
      plyr::ldply(function(name) data.frame(parameter = name, value = groups[[name]][1], low =  NA, high = NA, psa = NA))
  } else {
    group_vars <- NULL
  }
  if((class(data) %in% "data.frame") && (nrow(data) > 0)) {
    if(is.null(data$psa)) data$psa <- ""
    user_pars <- dplyr::transmute(
      data,
      parameter = name,
      value = value,
      low = ifelse(low == "", NA, low),
      high = ifelse(high == "", NA, high),
      psa = ifelse(psa == "", NA, psa)
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
  if ("from" %in% colnames(data)) {
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
    if ("state" %in% colnames(data)) {
      # Custom PSM
      data %>%
        dplyr::rowwise() %>%
        dplyr::do({
          if(.$strategy == "All") {
            data.frame(
              .model = strategies,
              state = .$state,
              prob = .$value,
              stringsAsFactors=F
            )
          } else {
            data.frame(
              .model = .$strategy,
              state = .$state,
              prob = .$value,
              stringsAsFactors=F
            )
          }
        }) %>%
        dplyr::ungroup()
    } else {
      # Regular PSM
      dplyr::rename(data, .model = strategy)
    }
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

hero_extract_psa_summ <- function(res, summ) {
  
  strategies <- unique(res$.strategy_names)
  n_strat <- length(strategies)
  
  indices <- expand.grid(referent = seq_len(n_strat), comparator = seq_len(n_strat)) %>%
    dplyr::filter(referent != comparator)
  value_names <- setdiff(colnames(res), c(".strategy_names", ".index"))
  
  ref_res <- res[indices$referent, ]
  comp_res <- res[indices$comparator, ]
  delta_res <- ref_res
  delta_res[value_names] <- ref_res[value_names] - comp_res[value_names]
  delta_res$.strategy_names <- paste0(ref_res$.strategy_names, " vs. ", comp_res$.strategy_names)
  all_res <- rbind(res, delta_res) %>%
    reshape2::melt(id.vars = c(".strategy_names", ".index")) %>%
    dplyr::mutate(variable = as.character(variable))
  
  summ_unique <- dplyr::distinct(summ, name, value)
  
  undisc <- dplyr::inner_join(
    dplyr::rename(summ_unique,variable = value),
    all_res,
    by = "variable"
  ) %>%
    dplyr::mutate(
      sim = .index,
      outcome = name,
      series = .strategy_names,
      group = variable,
      disc = F
    ) %>%
    dplyr::select(outcome, series, sim, group, disc, value)
  
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
      sim = .index,
      outcome = name,
      series = .strategy_names,
      group = variable,
      disc = T
    ) %>%
    dplyr::select(outcome, series, sim, group, disc, value)
  rbind(disc, undisc)
}

hero_extract_dsa_summ <- function(res, bc_res, summ) {
  
  bc_res_summs <- bc_res %>%
    dplyr::group_by(outcome,series, disc) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()
    
  bc_res_all <- plyr::rbind.fill(
    bc_res %>% dplyr::mutate(outcome=group),
    bc_res_summs
  ) %>%
  dplyr::transmute(
    outcome,
    series,
    disc,
    base = value
  )

  value_res <- as.data.frame(res$dsa, stringsAsFactors=F)
  value_res$.type <- rep(c("high", "low"), nrow(value_res)/2)
  
  strategies <- unique(value_res$.strategy_names)
  n_strat <- length(strategies)
  
  indices <- expand.grid(referent = seq_len(n_strat), comparator = seq_len(n_strat)) %>%
    dplyr::filter(referent != comparator)
  value_names <- setdiff(colnames(value_res), c(".strategy_names", ".par_names", ".par_value", ".par_value_eval", ".type"))
  
  delta_res <- plyr::ddply(indices, c("referent", "comparator"), function(x) {
    comp_res <- dplyr::filter(value_res, .strategy_names == strategies[x$comparator])
    ref_res <- dplyr::filter(value_res, .strategy_names == strategies[x$referent])
    delta <- ref_res
    delta[value_names] <- ref_res[value_names] - comp_res[value_names]
    delta$.strategy_names <- paste0(ref_res$.strategy_names, " vs. ", comp_res$.strategy_names)
    delta
  }) %>%
    dplyr::select(-referent, -comparator)
  all_res <- rbind(value_res, delta_res) %>%
    reshape2::melt(id.vars = c(".strategy_names", ".par_names", ".type", ".par_value", ".par_value_eval")) %>%
    dplyr::mutate(variable = as.character(variable))
  
  summ_unique <- tibble::tibble(outcome = unique(c(summ$value, summ$name)))
  
  undisc <- dplyr::inner_join(
    dplyr::rename(summ_unique,variable = outcome),
    all_res,
    by = "variable"
  ) %>%
    dplyr::mutate(
      param = .par_names,
      type = .type,
      param_value = .par_value,
      outcome = variable,
      series = .strategy_names,
      disc = F
    ) %>%
    dplyr::select(param, type, param_value, outcome, series, disc, value)
  
  disc <- dplyr::inner_join(
    dplyr::mutate(
      summ_unique,
      variable1 = paste0(".disc_", outcome),
      variable = outcome
    ) %>%
      dplyr::select(-outcome),
    all_res,
    by = c("variable1" = "variable")
  ) %>%
    dplyr::mutate(
      param = .par_names,
      type = .type,
      param_value = .par_value,
      outcome = variable,
      series = .strategy_names,
      group = variable,
      disc = T
    ) %>%
    dplyr::select(param, type, param_value, outcome, series, disc, value)
  rbind(disc, undisc) %>%
    reshape2::dcast(param+outcome+series+disc~type, value.var = "value") %>%
    dplyr::left_join(bc_res_all, by = c("outcome", "series", "disc"))
}
hero_extract_dsa_ce <- function(res, hsumms, esumms) {
  
  value_res <- as.data.frame(res$dsa, stringsAsFactors=F)
  value_res$.type <- rep(c("high", "low"), nrow(value_res)/2)
  
  unique_hsumms <- paste0(".disc_", unique(hsumms$name))
  unique_esumms <- paste0(".disc_", unique(esumms$name))
  
  expand.grid(
    hsumm = unique_hsumms,
    esumm = unique_esumms,
    stringsAsFactors = F
  ) %>%
    plyr::ddply(c("hsumm","esumm"), function(x){
      temp_res <- value_res
      temp_res$.cost <- value_res[[x$esumm]]
      temp_res$.effect <- value_res[[x$hsumm]]
      thresh <- hsumms %>% dplyr::filter(name == substring(x$hsumm, 7)) %>% .$wtp %>% .[1]
      plyr::ddply(temp_res, c(".par_names", ".type"), function(x) {
        compute_icer(x, threshold = thresh)
      })
    }) %>%
    dplyr::transmute(
      param = .par_names,
      type = .type,
      param_value = .par_value,
      health_outcome = hsumm,
      econ_outcome = esumm,
      series = .strategy_names,
      cost = .cost,
      eff = .effect,
      dcost = .dcost,
      deffect = .deffect,
      dref = .dref,
      icer = .icer,
      nmb = .nmb
    )
  
}
hero_extract_dsa_nmb <- function(hsumm_res, esumm_res, bc_res, hsumms, esumms) {
  
  distinct_hsumms <- dplyr::distinct(hsumms, name, wtp)
  distinct_esumms <- dplyr::distinct(esumms, name)
  
  # Get NMBs for each DSA scenario
  h_nmb <- hsumm_res %>%
    dplyr::filter(disc) %>%
    dplyr::inner_join(distinct_hsumms, by = c("outcome" = "name")) %>%
    dplyr::transmute(
      param,
      outcome,
      series,
      low = low * wtp,
      high = high * wtp,
      base = base * wtp
    )
  
  e_nmb <- esumm_res %>%
    dplyr::filter(disc) %>%
    dplyr::inner_join(distinct_esumms, by = c("outcome" = "name")) %>%
    dplyr::transmute(
      param,
      outcome,
      series,
      low = -low,
      high = -high,
      base = -base
    )
  
  expand.grid(
    health_outcome = unique(hsumms$name),
    econ_outcome = unique(esumms$name),
    stringsAsFactors = F
  ) %>%
    plyr::ddply(c("health_outcome", "econ_outcome"), function(x) {
      rbind(
        h_nmb %>% dplyr::filter(outcome == x$health_outcome),
        e_nmb %>% dplyr::filter(outcome == x$econ_outcome)
      )
    }) %>%
    dplyr::group_by(param, series, health_outcome, econ_outcome) %>%
    dplyr::summarise(low = sum(low), high = sum(high), base = sum(base))
  
}

compile_parameters <- function(x) {
  if (is.null(x$demographics)) {
    # Homogenous model
    lapply(x$model_runs$eval_strategy_list, function(x) x$parameters) %>%
      do.call(rbind, .) %>%
      as.tbl()
  } else {
    # Heterogeneous model
    lapply(x$demographics$model_list, function(x) {
      lapply(x$.mod, function(x) x$parameters)
    }) %>%
      unlist(recursive=F) %>%
      do.call(rbind, .) %>%
      as.tbl()
  }
}

compile_values <- function(x) {
  if (is.null(x$demographics)) {
    
    models <- x$model_runs$eval_strategy_list
    
    strategy_names <- names(models)
    n_strategy <- length(strategy_names)
    
    state_names <- names(models[[1]]$states)
    n_state <- length(state_names)
    
    value_names <- colnames(models[[1]]$states[[1]])[-1]
    
    states_list <- vector(mode = "list", length = n_strategy * n_state)
    trans_list <- vector(mode = "list", length = n_strategy)
    state_count <- 1
    trans_count <- 1
    
    lapply(seq_len(n_strategy), function(i) {
      trans_df <- attr(models[[i]]$states, "transitions")
      if(!is.null(trans_df)) {
        trans_df$strategy <- strategy_names[[i]]
        trans_list[[trans_count]] <<- trans_df
        trans_count <<- trans_count + 1
      }
      
      lapply(seq_len(n_state), function(k) {
        state_df <- models[[i]]$states[[k]]
        state_df$strategy <- strategy_names[[i]]
        state_df$state <- state_names[[k]]
        states_list[[state_count]] <<- state_df
        state_count <<- state_count + 1
      })
    })
    
    trans_df <- data.table::rbindlist(trans_list)
    if(nrow(trans_df) > 0) {
      trans_df <- trans_df %>%
        dplyr::mutate(state = paste0(.from_name_expanded, "→", .to_name_expanded)) %>%
        data.table::data.table() %>%
        data.table::dcast(strategy+state+markov_cycle~variable, value.var = "value")
    } else {
      trans_df <- NULL
    }
    out_df <- rbind(data.table::rbindlist(states_list), trans_df) %>%
      dplyr::arrange(strategy, state, markov_cycle) %>%
      dplyr::rename(cycle = markov_cycle)
    
    out_df[ ,c("strategy", "state", "cycle", value_names), drop = F]
  } else {
    # Heterogeneous model
    
    models <- x$demographics$model_list
    
    strategy_names <- names(models)
    n_strategy <- length(strategy_names)
    
    group_names <- as.character(lapply(models[[1]]$.mod, function(x) x$parameters$.group[1]))
    n_group <- length(group_names)
    
    state_names <- names(models[[1]]$.mod[[1]]$states)
    n_state <- length(state_names)
    
    value_names <- colnames(models[[1]]$.mod[[1]]$states[[1]])[-1]
    
    states_list <- vector(mode = "list", length = n_strategy * n_group * n_state)
    trans_list <- vector(mode = "list", length = n_strategy * n_group)
    state_count <- 1
    trans_count <- 1
    
    states_df <- lapply(seq_len(n_strategy), function(i) {
      lapply(seq_len(n_group), function(j) {
        trans_df <- attr(models[[i]]$.mod[[j]]$states, "transitions")
        if(!is.null(trans_df)) {
          trans_df$strategy <- strategy_names[[i]]
          trans_df$group <- group_names[[j]]
          trans_list[[trans_count]] <<- trans_df
          trans_count <<- trans_count + 1
        }
        
        lapply(seq_len(n_state), function(k) {
          state_df <- models[[i]]$.mod[[j]]$states[[k]]
          state_df$strategy <- strategy_names[[i]]
          state_df$group <- group_names[[j]]
          state_df$state <- state_names[[k]]
          states_list[[state_count]] <<- state_df
          state_count <<- state_count + 1
        })
      })
    })
    trans_df <- data.table::rbindlist(trans_list)
    if(nrow(trans_df) > 0) {
      trans_df <- trans_df %>%
        dplyr::mutate(state = paste0(.from_name_expanded, "→", .to_name_expanded)) %>%
        data.table::data.table() %>%
        data.table::dcast(strategy+group+state+markov_cycle~variable, value.var = "value")
    } else {
      trans_df <- NULL
    }
    out_df <- rbind(data.table::rbindlist(states_list), trans_df) %>%
      dplyr::arrange(strategy, group, state, markov_cycle) %>%
      dplyr::rename(cycle = markov_cycle)
    
      out_df[ ,c("strategy", "group", "state", "cycle", value_names), drop = F]
  }
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
        dplyr::select_(.dots = c("strategy", "cycle", "pfs", "os")) %>%
        as.tbl()
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
            dplyr::mutate(
              from = rep(state_names, n_cycles),
              cycle = rep(seq_len(n_cycles), each = n_states)
            )
        }, .id = "strategy") %>%
          dplyr::select_(.dots = c("strategy", "cycle", "from", state_names)) %>%
          as.tbl()
      }
    }
  } else {
    # Heterogeneous model
    the_class <- class(x$demographics$model_list[[1]]$.mod[[1]]$transition[[1]])
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
        dplyr::select_(.dots = c("strategy", "group", "cycle", "pfs", "os")) %>%
        as.tbl()
      
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
              dplyr::mutate(
                from = rep(state_names, n_cycles),
                cycle = rep(seq_len(n_cycles), each = n_states)
              )
          }, .id = "group")
        }, .id = "strategy") %>%
          dplyr::select_(.dots = c("strategy", "group", "cycle", "from", state_names)) %>%
          as.tbl()
      }
    }
  }
  
}

#' @export
run_hero_model <- function(...) {
  
  if(type %in% c("base case", "export") || (type == "vbp" && nrow(as.data.frame(groups)) <= 1)) {
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
    method <- "life-table"
    if (!is.null(dots$method)) {
      method <- dots$method
    }
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
        "method", method,
        "cycles", max(1, round(settings$n_cycles,0)),
        "n",      1000,
        "init",   paste(states$prob,collapse=", ")
      ),
      data = tables,
      run_dsa = F,
      run_psa = F,
      run_demo = !(is.null(groups_tbl)),
      state_time_limit = limits,
      source = scripts,
      aux_params = surv
    )
    if(is.null(groups_tbl)) {
      main_res <- heemod_res$model_runs
    } else {
      main_res <- heemod_res$demographics$combined_model
    }

    if(type == "vbp") {
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
    } else if(type == "base case") {
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
    } else if(type == "export") {
      health_res <- hero_extract_summ(main_res, hsumms)
      econ_res <- hero_extract_summ(main_res, esumms)
      nmb_res <- hero_extract_nmb(health_res, econ_res, hsumms) %>%
        dplyr::rename(
          "Outcome" = outcome,
          "Strategy" = series,
          "Component" = group,
          "Type" = type,
          "Value" = value
        ) %>%
        dplyr::select(-disc)
      health_res <- health_res %>%
        dplyr::rename(
          "Outcome" = outcome,
          "Strategy" = series,
          "Component" = group,
          "Discounted" = disc,
          "Value" = value
        )
      econ_res <- econ_res %>%
        dplyr::rename(
          "Outcome" = outcome,
          "Strategy" = series,
          "Component" = group,
          "Discounted" = disc,
          "Value" = value
        )
      ce_res <- hero_extract_ce(main_res, hsumms, esumms) %>%
        dplyr::mutate(
          health_outcome = substring(health_outcome, 7),
          econ_outcome = substring(econ_outcome, 7)
        ) %>%
        dplyr::rename(
          "Health Outcome" = health_outcome,
          "Economic Outcome" = econ_outcome,
          "Strategy" = series,
          "Cost" = cost,
          "Effect" = eff,
          "Δ Cost" = dcost,
          "Δ Effect" = deffect,
          "Reference" = dref,
          "ICER" = icer
        ) %>%
        dplyr::select(-hsumm, esumm)
      trace_res <- hero_extract_trace(main_res) %>%
        dplyr::rename(
          "Day" = model_day,
          "Week" = model_week,
          "Month" = model_month,
          "Year" = model_year,
          "Strategy" = series
        )
      param_res <- compile_parameters(heemod_res)
      trans_res <- compile_transitions(heemod_res)
      values_res <- compile_values(heemod_res)
      if(length(tables) > 0) {
        tables_list <- tables
        names(tables_list) <- paste0("Tbl - ", names(tables))
      } else {
        tables_list <- list()
      }
      wb_list <- list(
        "Inputs - Decision" = data.frame("Decision" = decision),
        "Input - Settings" = settings,
        "Input - Groups" = groups,
        "Input - Strategies" = strategies,
        "Inputs - States" = states,
        "Inputs - Transitions" = transitions,
        "Inputs - Health Values" = hvalues,
        "Inputs - Econ Values" = evalues,
        "Inputs - Health Summ" = hsumms,
        "Inputs - Econ Summ" = esumms,
        "Inputs - Parameters" = variables,
        "Inputs - Surv Dists" = surv_dists
      ) %>%
        append(tables_list) %>%
        append(list(
          "Calc - Params"= param_res,
          "Calc - Trans"= trans_res,
          "Calc - Values"= values_res,
          "Results - Trace" = trace_res,
          "Results - Outcomes" = health_res,
          "Results - Costs" = econ_res,
          "Results - CE" = ce_res,
          "Results - NMB" = nmb_res
        )) %>%
        purrr::keep(~(ncol(.) > 0) && (nrow(.) > 0))
      writeWorkbook(lapply(wb_list, as.data.frame), "model.xlsx")
      ret <- wb_list
    }
  } else if(type == "vbp") {
    
    # Workaround:
    # To run VBP with a heterogenous model, need to run
    # vbp separately for each group then aggregate.
    
    vbps <- plyr::alply(groups, 1, function(x) {
      group_model <- run_hero_model(
        decision = decision,
        settings = settings,
        groups = x,
        strategies = strategies,
        states = states,
        transitions = transitions,
        hvalues = hvalues,
        evalues = evalues,
        hsumms = hsumms,
        esumms = esumms,
        variables = variables,
        tables = tables,
        scripts = scripts,
        cost = cost,
        effect = effect,
        surv_dists = surv_dists,
        type = "vbp",
        vbp = vbp
      )
      eq <- group_model$eq
      eq$a <- eq$a * as.numeric(x$weight)
      eq$b <- eq$b * as.numeric(x$weight)
      eq
    })
    average_vbp <- vbps[[1]]
    weights <- as.numeric(groups$weight)
    average_vbp$a <- Reduce(`+`, purrr::map(vbps, ~ .$a)) / sum(weights)
    average_vbp$b <- Reduce(`+`, purrr::map(vbps, ~ .$b)) / sum(weights)
    ret <- list(eq = average_vbp)
  }
  
  ret
}

build_hero_model <- function(...) {
  
  # Capture arguments
  dots <- list(...)
  
  if (is.null(dots$psa)) {
    dots$psa <- list(
      n = 1
    )
  }
  
  # Format parameters Table
  params <- parse_hero_vars(
    dots$variables,
    dots$settings$cycle_length,
    dots$settings$disc_eff,
    dots$settings$disc_cost,
    dots$groups
  )
  
  # Format survival distributions table
  surv <- parse_hero_obj_vars(dots$surv_dists)
  
  # Format groups table
  groups_tbl <- parse_hero_groups(dots$groups)
  
  # Format transitions table
  trans <- parse_hero_trans(dots$transitions, dots$strategies$name)
  
  # Format state list
  state_list <- parse_hero_states(
    dots$hvalues,
    dots$evalues,
    dots$hsumms,
    dots$esumms,
    dots$strategies$name,
    dots$states$name,
    dots$settings$cycle_length
  )
  
  # Format state transitions list
  st_list <- parse_hero_states_st(
    dots$hvalues,
    dots$evalues,
    dots$hsumms,
    dots$esumms,
    dots$strategies$name,
    dots$states$name,
    dots$settings$cycle_length
  )
  
  # Format state time limits
  limits <- as.numeric(dots$states$limit)
  names(limits) <- dots$states$name
  limits <- limits[!is.na(limits) & !(limits == 0)]
  
  # Determine half-cycle method
  method <- "life-table"
  if(!is.null(dots$settings$method)) {
    method <- dots$settings$method
  }
  
  # Return model object
  list(
    states = state_list,
    st = st_list,
    tm = trans,
    param = params,
    demo = groups_tbl,
    options = tibble::tribble(
      ~option,  ~value,
      "cost",   paste0(".disc_", dots$esumms$name[1]),
      "effect", paste0(".disc_", dots$hsumms$name[1]),
      "method", method,
      "cycles", max(1, round(dots$settings$n_cycles,0)),
      "n",      dots$psa$n,
      "init",   paste(dots$states$prob,collapse=", ")
    ),
    data = dots$tables,
    state_time_limit = limits,
    source = dots$scripts,
    aux_params = surv,
    psa = dots$psa
  )
}

#' @export
run_hero_bc <- function(...) {
  
  # Capture arguments
  dots <- list(...)
  
  # Compile model object
  args <- do.call(build_hero_model, dots)
  args$run_demo = !(is.null(args$demo))
  
  # Run model
  heemod_res <- do.call(run_model_api, args)
  
  # Extract Main Results
  if(is.null(heemod_res$demographics)) {
    main_res <- heemod_res$model_runs
  } else {
    main_res <- heemod_res$demographics$combined_model
  }
  
  # Format Results Types
  health_res <- hero_extract_summ(main_res, dots$hsumms)
  econ_res <- hero_extract_summ(main_res, dots$esumms)
  nmb_res <- hero_extract_nmb(health_res, econ_res, dots$hsumms)
  ce_res <- hero_extract_ce(main_res, dots$hsumms, dots$esumms)
  trace_res <- hero_extract_trace(main_res)
  
  # Return
  list(
    trace = trace_res,
    outcomes = health_res,
    costs = econ_res,
    ce = ce_res,
    nmb = nmb_res
  )
}

#' @export
run_hero_vbp <- function(...) {
  
  # Capture arguments
  dots <- list(...)
  
  if(nrow(as.data.frame(dots$groups)) <= 1) {
    # Homogenous model
    # Compile model object
    args <- do.call(build_hero_model, dots)
    
    # Use cost/effect specified by vbp config
    args$options$value[args$options$option == "cost"] <- paste0(".disc_", dots$vbp$cost)
    args$options$value[args$options$option == "effect"] <- paste0(".disc_", dots$vbp$effect)
    
    # Run base case
    heemod_res <- do.call(run_model_api, args)
    
    # Extract Main Results
    main_res <- heemod_res$model_runs
    
    # Run VBP
    vbp_low <-  lazyeval::as.lazy_dots(setNames(list(0), dots$vbp$par_name), environment())
    vbp_med <-  lazyeval::as.lazy_dots(setNames(list(dots$vbp$wtp/2), dots$vbp$par_name), environment())
    vbp_high <-  lazyeval::as.lazy_dots(setNames(list(dots$vbp$wtp), dots$vbp$par_name), environment())
    vbp_settings <- define_vbp_(dots$vbp$par_name, vbp_low, vbp_med, vbp_high)
    vbp_res <- run_vbp(
      model = main_res,
      vbp = vbp_settings,
      strategy_vbp = dots$vbp$strat,
      wtp_thresholds = c(0, 100000)
    )
    eq <- vbp_res$lin_eq
    
  } else {
    # Heterogeneous model
    # Run VBP analysis for each group
    vbps <- plyr::alply(dots$groups, 1, function(x) {
      
      # Run model for given group
      group_args <- dots
      group_args$groups <- x
      group_model <- do.call(run_hero_vbp, group_args)
      
      # Extract linear equation and apply weight
      eq <- group_model$eq
      eq$a <- eq$a * as.numeric(x$weight)
      eq$b <- eq$b * as.numeric(x$weight)
      eq
    })
    
    # Aggregate results over all groups
    average_vbp <- vbps[[1]]
    weights <- as.numeric(dots$groups$weight)
    average_vbp$a <- Reduce(`+`, purrr::map(vbps, ~ .$a)) / sum(weights)
    average_vbp$b <- Reduce(`+`, purrr::map(vbps, ~ .$b)) / sum(weights)
    eq <- average_vbp
  }
  
  # Return Result
  list(
    eq = eq
  )
  
}

#' @export
run_hero_dsa <- function(...) {
  
  # Capture arguments
  dots <- list(...)
  
  if(nrow(as.data.frame(dots$groups)) <= 1) {
    # Homogenous model
    # Compile model object
    args <- do.call(build_hero_model, dots)
    args$run_dsa = TRUE
    
    # Run Model
    heemod_res <- do.call(run_model_api, args)
    
    # Extract BC results
    
    bc_outcome_res <- hero_extract_summ(heemod_res$model_runs, dots$hsumms)
    bc_cost_res <- hero_extract_summ(heemod_res$model_runs, dots$esumms)
    bc_nmb_res <- hero_extract_nmb(bc_outcome_res, bc_cost_res, dots$hsumms)
    bc_ce_res <- hero_extract_ce(heemod_res$model_runs, dots$hsumms, dots$esumms)
 
    # Extract DSA results
    outcome_res <- hero_extract_dsa_summ(heemod_res$dsa, bc_outcome_res, dots$hsumms)
    cost_res <- hero_extract_dsa_summ(heemod_res$dsa, bc_cost_res, dots$esumms)
    ce_res <- hero_extract_dsa_ce(heemod_res$dsa, dots$hsumms, dots$esumms)
    nmb_res <- hero_extract_dsa_nmb(outcome_res, cost_res, bc_nmb_res, dots$hsumms, dots$esumms)
    
    ret <- list(
      main = as.data.frame(heemod_res$dsa$dsa, stringsAsFactors=F),
      outcomes = outcome_res,
      cost = cost_res,
      ce = ce_res,
      nmb = nmb_res
    )
    
  } else {
    # Heterogeneous model
    # Run DSA analysis for each group
    dsas <- plyr::alply(dots$groups, 1, function(x) {
      
      # Run model for given group
      group_args <- dots
      group_args$groups <- x
      group_model <- do.call(run_hero_dsa, group_args)
      
      # Extract results and apply weights
      group_model$outcomes$base <- group_model$outcomes$base * as.numeric(x$weight)
      group_model$outcomes$low <- group_model$outcomes$low * as.numeric(x$weight)
      group_model$outcomes$high <- group_model$outcomes$high * as.numeric(x$weight)
      group_model$cost$base <- group_model$cost$base * as.numeric(x$weight)
      group_model$cost$low <- group_model$cost$low * as.numeric(x$weight)
      group_model$cost$high <- group_model$cost$high * as.numeric(x$weight)
      group_model$nmb$base <- group_model$nmb$base * as.numeric(x$weight)
      group_model$nmb$low <- group_model$nmb$low * as.numeric(x$weight)
      group_model$nmb$high <- group_model$nmb$high * as.numeric(x$weight)
      group_model$ce$.cost <- group_model$ce$cost * as.numeric(x$weight)
      group_model$ce$.effect <- group_model$ce$eff * as.numeric(x$weight)
      
      group_model
    })
    
    # Aggregate results over all groups
    
    weights <- as.numeric(dots$groups$weight)
    nmb_res <- dsas[[1]]$nmb
    nmb_res$low <- Reduce(`+`, purrr::map(dsas, ~ .$nmb$low)) / sum(weights)
    nmb_res$high <- Reduce(`+`, purrr::map(dsas, ~ .$nmb$high)) / sum(weights)
    nmb_res$base <- Reduce(`+`, purrr::map(dsas, ~ .$nmb$base)) / sum(weights)
    
    outcomes_res <- dsas[[1]]$outcomes
    outcomes_res$low <- Reduce(`+`, purrr::map(dsas, ~ .$outcomes$low)) / sum(weights)
    outcomes_res$high <- Reduce(`+`, purrr::map(dsas, ~ .$outcomes$high)) / sum(weights)
    outcomes_res$base <- Reduce(`+`, purrr::map(dsas, ~ .$outcomes$base)) / sum(weights)
    
    cost_res <- dsas[[1]]$cost
    cost_res$low <- Reduce(`+`, purrr::map(dsas, ~ .$cost$low)) / sum(weights)
    cost_res$high <- Reduce(`+`, purrr::map(dsas, ~ .$cost$high)) / sum(weights)
    cost_res$base <- Reduce(`+`, purrr::map(dsas, ~ .$cost$base)) / sum(weights)
    
    ce_res <- dsas[[1]]$ce
    ce_res$.cost <- Reduce(`+`, purrr::map(dsas, ~ .$ce$.cost)) / sum(weights)
    ce_res$.eff <- Reduce(`+`, purrr::map(dsas, ~ .$ce$.eff)) / sum(weights)
    ce_res %>%
      plyr::ddply(c("health_outcome","econ_outcome", "param", "type"), function(x) {
        thresh <- dots$hsumms %>% dplyr::filter(name == substring(x$health_outcome,7)[1]) %>% .$wtp %>% .[1]
        dplyr::mutate(x, .strategy_names = series) %>%
          compute_icer(threshold = thresh)
      }) %>%
      dplyr::transmute(
        param = param,
        type = type,
        param_value = param_value,
        health_outcome = health_outcome,
        econ_outcome = econ_outcome,
        series = .strategy_names,
        cost = .cost,
        eff = .effect,
        dcost = .dcost,
        deffect = .deffect,
        dref = .dref,
        icer = .icer,
        nmb = .nmb
      )
    
    ret <- list(
      outcomes = outcomes_res,
      cost = cost_res,
      ce = ce_res,
      nmb = nmb_res
    )

  }
  ret
}

#' @export
run_hero_psa <- function(...) {
  
  
  # Capture arguments
  dots <- list(...)
  
  if(nrow(as.data.frame(dots$groups)) <= 1) {
    # Homogenous model
    # Compile model object
    args <- do.call(build_hero_model, dots)
    args$run_psa <- T
    
    # Run Model
    psa_model <- do.call(run_model_api, args)
  } else {
    # Heterogeneous model
    # Run PSA analysis for each group
    the_seed <- sample.int(99999, 1)
    psas <- plyr::alply(dots$groups, 1, function(x) {
      
      # Run model for given group
      group_args <- dots
      group_args$groups <- x
      set.seed(the_seed)
      args <- do.call(build_hero_model, group_args)
      args$run_psa <- T
      do.call(run_model_api, args)
    })
    
    psa_model <- psas[[1]]
    psa_res_df <- plyr::ldply(psas, function(x) x$psa$psa)
    col_indices <- setdiff(colnames(psa_res_df), c(".strategy_names", ".index", "name", "weight"))
    psa_res_df[ , col_indices] <- psa_res_df[ , col_indices] * psa_res_df$weight
    psa_model$psa$psa <- psa_res_df %>%
      dplyr::select(-name) %>%
      dplyr::group_by(.strategy_names, .index) %>%
      dplyr::summarize_all(sum)
  }
  
  thresh_max <- dots$psa$thresh_max
  thresh_step <- dots$psa$thresh_step
  thresh_n_steps <- thresh_max / thresh_step
  
  outcomes <- hero_extract_psa_summ(psa_model$psa$psa, dots$hsumms)
  costs <- hero_extract_psa_summ(psa_model$psa$psa, dots$esumms)
  ceac <- acceptability_curve(psa_model$psa$psa, seq(from = 0,to = dots$psa$thresh_max,by = thresh_step)) %>%
    reshape2::dcast(.ceac~.strategy_names, value.var = ".p") %>%
    dplyr::rename(wtp = .ceac)
  evpi <- compute_evpi(psa_model$psa, seq(from = 0, to = dots$psa$thresh_max, by = thresh_step)) %>%
    dplyr::rename(wtp = .ceac, value = .evpi)
  # evppi <- compute_evppi(
  #   psa_model$psa,
  #   define_evppi_(psa_model$psa$resamp_par),
  #   max_wtp = dots$psa$thresh_max,
  #   n = thresh_n_steps + 1,
  #   verbose = F
  # )$evppi_res %>%
  #   dplyr::rename(wtp = WTP) %>%
  #   reshape2::melt(id.vars = "wtp", value.name = "value")
  # 
  list(
    outcomes = outcomes,
    costs = costs,
    ceac = ceac,
    evpi = evpi#,
    #evppi = evppi
  )
}

#' @export
export_hero_xlsx <- function(...) {
  
  # Capture arguments
  dots <- list(...)
  
  # Compile model object
  args <- do.call(build_hero_model, dots)
  args$run_demo = !(is.null(args$demo))
  
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
    dplyr::rename(
      "Outcome" = outcome,
      "Strategy" = series,
      "Component" = group,
      "Type" = type,
      "Value" = value
    ) %>%
    dplyr::select(-disc)
  health_res <- health_res %>%
    dplyr::rename(
      "Outcome" = outcome,
      "Strategy" = series,
      "Component" = group,
      "Discounted" = disc,
      "Value" = value
    )
  econ_res <- econ_res %>%
    dplyr::rename(
      "Outcome" = outcome,
      "Strategy" = series,
      "Component" = group,
      "Discounted" = disc,
      "Value" = value
    )
  ce_res <- hero_extract_ce(main_res, dots$hsumms, dots$esumms) %>%
    dplyr::mutate(
      health_outcome = substring(health_outcome, 7),
      econ_outcome = substring(econ_outcome, 7)
    ) %>%
    dplyr::rename(
      "Health Outcome" = health_outcome,
      "Economic Outcome" = econ_outcome,
      "Strategy" = series,
      "Cost" = cost,
      "Effect" = eff,
      "Δ Cost" = dcost,
      "Δ Effect" = deffect,
      "Reference" = dref,
      "ICER" = icer
    ) %>%
    dplyr::select(-hsumm, esumm)
  trace_res <- hero_extract_trace(main_res) %>%
    dplyr::rename(
      "Day" = model_day,
      "Week" = model_week,
      "Month" = model_month,
      "Year" = model_year,
      "Strategy" = series
    )
  param_res <- compile_parameters(heemod_res)
  trans_res <- compile_transitions(heemod_res)
  values_res <- compile_values(heemod_res)
  if(length(dots$tables) > 0) {
    tables_list <- dots$tables
    names(tables_list) <- paste0("Tbl - ", names(dots$tables))
  } else {
    tables_list <- list()
  }
  wb_list <- list(
    "Inputs - Decision" = data.frame("Decision" = dots$decision),
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
      "Calc - Values"= values_res,
      "Results - Trace" = trace_res,
      "Results - Outcomes" = health_res,
      "Results - Costs" = econ_res,
      "Results - CE" = ce_res,
      "Results - NMB" = nmb_res
    )) %>%
    purrr::keep(~(ncol(.) > 0) && (nrow(.) > 0))
  writeWorkbook(lapply(wb_list, as.data.frame), paste0(dots$name, ".xlsx"))
  ret <- wb_list
  
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

package_hero_model <- function(...) {
  dots <- list(...)
  model_object <- list(
    decision = dots$decision,
    settings = dots$settings,
    groups = dots$groups,
    strategies = dots$strategies,
    states = dots$states,
    transitions = dots$transitions,
    hvalues = dots$hvalues,
    evalues = dots$evalues,
    hsumms = dots$hsumms,
    esumms = dots$esumms,
    variables = dots$variables,
    tables = dots$tables,
    scripts = dots$scripts,
    surv_dists = dots$surv_dists,
    type = dots$type,
    vbp = dots$vbp,
    psa = dots$psa
  )
  rproj_string <- "Version: 1.0
RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default
EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 4
Encoding: UTF-8
RnwWeave: knitr
LaTeX: pdfLaTeX"
  rcode_string <- "if(!require(heRomod)) {
  if(!require(devtools)) {
    install.packages('devtools')
  }
  library(devtools)
  install_github('PolicyAnalysisInc/heRomod')
  library(heRomod)
}
model <- readRDS('./model.rds')
results <- do.call(run_hero_bc, model)
"
  write(rproj_string, paste0(dots$name, ".rproj"))
  write(rcode_string, "run.R")
  saveRDS(model_object, "model.rds")
  utils::zip(
    paste0(dots$name, ".zip"),
    c(paste0(dots$name, ".rproj"), "run.R", "model.rds")
  )
}

writeWorkbook <- function(dflist, path, ...){
  sheet_names <- strtrim(names(dflist), 31)
  wb <- openxlsx::createWorkbook()
  for(i in 1:length(dflist)){
    openxlsx::addWorksheet(wb, sheet_names[i])
    openxlsx::writeDataTable(wb, sheet_names[i], dflist[[i]],...)
    openxlsx::setColWidths(wb, sheet_names[i], cols = seq_len(ncol(dflist[[i]])), widths = 24)
    openxlsx::freezePane(wb, sheet_names[i], firstActiveRow = 2, firstActiveCol = 1)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}
