#' @export
run_analysis <- function(...) {
  data <- list(...)
  manifest <- create_manifest()
  data$.manifest <- manifest
  runner <- switch(
    data$analysis,
    'psa' = run_hero_psa,
    'dsa' = run_hero_dsa,
    'twsa' = run_hero_twsa,
    'vbp' = run_hero_vbp,
    'bc' = run_hero_bc,
    'scen' = run_hero_scen,
    'excel' = export_hero_xlsx,
    'code_preview' = run_code_preview_compat,
    'r_project' = package_hero_model,
    stop('Parameter "analysis" must be one of: "bc", "vbp", "dsa", "twsa", "psa", "scen", "excel", "code_preview", "r_project".')
  )
  res <- try({ do.call(runner, convert_model(data)) })
  if (inherits(res, "try-error")) {
    msg <- gsub('Error : ', fixed = T, '', res)
    res <- list(
      error = paste0('Error: ', as.character(msg))
    )
  }
  res$warnings <- paste(capture.output(warnings()), collapse = '\n')

  # Write Results to JSON
  filename <- 'results.json'
  jsonlite::write_json(res, filename, digits = 12)
  no_default <- is.na(manifest$get_manifest()$default)
  manifest$register_file('main_results', filename, 'Main results of running analysis', default = no_default)
  
  list(
    content = res,
    manifest = manifest$get_manifest()
  )
}

create_manifest <- function() {
  manifest <- list(default = NA, files = list())
  list(
    get_manifest = function() manifest,
    register_file = function(id, path, description, default = F) {
      item <- list(path = path, description = description)
      manifest$files[[id]] <<- item
      if (default) {
        manifest$default <<- id
      }
    }
  )
}

parse_hero_settings <- function(settings) {
  
  # Determine half-cycle method
  if (is.null(settings$method)) {
    settings$method <- "life-table"
  }
  
  # Determine discounting method
  if (is.null(settings$disc_method)) {
    settings$disc_method <- 'start'
  }
  
  
  if (!is.null(settings$CycleLength)) {
    if (!is.null(settings$days_per_year)) {
      dpy <- settings$days_per_year
    } else {
      dpy <- 365
    }
    # Caclulate cycle length
    cl_n <- settings$CycleLength
    cl_u <- settings$CycleLengthUnits
    cl <- time_in_days(cl_u, dpy) * cl_n
    
    # Calculate timeframe
    tf_n <- settings$ModelTimeframe
    tf_u <- settings$ModelTimeframeUnits
    tf <- time_in_days(tf_u, dpy) * tf_n
    
    # Populate settings object with number of cycles
    settings$n_cycles <- max(1, round(tf / cl))
  }
  
  settings
}
parse_hero_vars <- function(data, settings, groups) {
  if (!is.null(settings$CycleLengthUnits)) {
    cl_u <- settings$CycleLengthUnits
    cl_n <- settings$CycleLength
    if (!is.null(settings$days_per_year)) {
      dpy <- settings$days_per_year
    } else {
      dpy <- 365
    }
    cl_d_formula <- str_interp('time_in_days("${cl_u}", ${dpy}) * ${cl_n}')
    cl_w_formula <- str_interp('cycle_length_days * time_in_days("days", ${dpy}) / time_in_days("weeks", ${dpy}) ')
    cl_m_formula <- str_interp('cycle_length_days * time_in_days("days", ${dpy}) / time_in_days("months", ${dpy}) ')
    cl_y_formula <- str_interp('cycle_length_days * time_in_days("days", ${dpy}) / time_in_days("years", ${dpy}) ')
    cl <- time_in_days(cl_u, dpy) * cl_n
  } else {
    dpy <- 365
    cl <- settings$cycle_length
    cl_d_formula <- as.character(cl)
    cl_w_formula <- "cycle_length_days / 7"
    cl_m_formula <- "cycle_length_days * 12 / 365"
    cl_y_formula <- "cycle_length_days / 365"
  }
  hdisc_adj <- rescale_discount_rate(settings$disc_eff, dpy, cl)
  edisc_adj <- rescale_discount_rate(settings$disc_cost, dpy, cl)
  hero_pars <- tibble::tribble(
    ~parameter,            ~value,                               ~low, ~high, ~psa,
    "cycle_length_days",   cl_d_formula,                           NA,    NA,   NA,
    "cycle_length_weeks",  cl_w_formula,                           NA,    NA,   NA,
    "cycle_length_months", cl_m_formula,                           NA,    NA,   NA,
    "cycle_length_years",  cl_y_formula,                           NA,    NA,   NA,
    "model_day",           "markov_cycle * cycle_length_days",     NA,    NA,   NA,
    "model_week",          "markov_cycle * cycle_length_weeks",    NA,    NA,   NA,
    "model_month",         "markov_cycle * cycle_length_months",   NA,    NA,   NA,
    "model_year",          "markov_cycle * cycle_length_years",    NA,    NA,   NA,
    "state_day",           "state_time * cycle_length_days",       NA,    NA,   NA,
    "state_week",          "state_time * cycle_length_weeks",      NA,    NA,   NA,
    "state_month",         "state_time * cycle_length_months",     NA,    NA,   NA,
    "state_year",          "state_time * cycle_length_years",      NA,    NA,   NA,
    "disc_h",              paste0("discount(1, ", hdisc_adj, ")"), NA,    NA,   NA,
    "disc_e",              paste0("discount(1, ", edisc_adj, ")"), NA,    NA,   NA
  )
  if((class(groups) %in% "data.frame") && (nrow(groups) > 0)) {
    groups <- groups %>%
      mutate(group = name) %>%
      rename(!!!syms(c(".group" = "name", '.group_weight' = 'weight')))
    group_vars <- groups %>%
      colnames() %>%
      plyr::ldply(function(name) data.frame(parameter = name, value = groups[[name]][1], low =  NA, high = NA, psa = NA))
  } else {
    group_vars <- data.frame(parameter = '.group_weight', value = '1', low =  NA, high = NA, psa = NA)
  }
  if((class(data) %in% "data.frame") && (nrow(data) > 0)) {
    if(is.null(data$psa)) data$psa <- ""
    user_pars <- transmute(
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
    transmute(
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
    rename(data, !!!syms(c(".group" = "name", ".weights" = "weight"))) %>%
    mutate(group = .group, .weights = .weights)
  } else {
    NULL
  }
}
parse_hero_values <- function(data, health, strategies, states) {
  
  trans_string <- "\u2192"
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
  state_vals <- data %>%
    filter(!grepl(trans_string, state, fixed = T), state != "Model Start")
  if (nrow(state_vals) == 0) {
    return(tibble(name = character(), .model = character(), .state = character(), value = character()))
  }
  states_undisc <- state_vals %>%
    rowwise() %>%
    do({
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
    ungroup() %>%
    group_by(name, .model) %>%
    do({
      df <- data.frame(., stringsAsFactors = F)
      isAllOther <- df$.state == "All Other"
      if (!any(isAllOther)) df
      else if (sum(isAllOther) > 1) {
        stop('Error in values, "All Other" may only be used once per value and strategy.', call. = F)
      } else {
        allOtherIndex <- which(isAllOther)
        isStateUsed <- states %in% df$.state
        unusedStates <- states[!isStateUsed]
        extraRows <- df[rep(allOtherIndex, length(unusedStates)), ]
        extraRows$.state <- unusedStates
        rows <- rbind(
          df[-allOtherIndex, ],
          extraRows,
          stringsAsFactors = F
        )
        rows
      }
    }) %>%
    ungroup()
  
  if(nrow(states_undisc) > 0) {
    states_disc <- states_undisc %>%
      mutate(
        value = disc_fun(name),
        name = paste0(".disc_", name)
      )
  } else {
    states_disc <- NULLs
  }
  rbind(states_undisc, states_disc)
}
parse_hero_values_start <- function(data, strategies) {
  
  start_vals <- data %>%
    filter(state == "Model Start")
  
  start_undisc <- start_vals %>%
    rowwise() %>%
    do({
      if(.$strategy == "All") {
        data.frame(
          name = .$name,
          .model = strategies,
          value = .$value,
          stringsAsFactors = F
        )
      } else {
        data.frame(
          name = .$name,
          .model = .$strategy,
          value = .$value,
          stringsAsFactors = F
        )
      }
    }) %>%
    ungroup()
  start_disc <- start_undisc
  if (nrow(start_disc) > 0) start_disc$name <- paste0(".disc_", start_disc$name)
  rbind(start_undisc, start_disc)
}
parse_hero_values_st <- function(data, health, strategies) {
  
  trans_string <- "\u2192"
  
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
  state_trans <- data %>%
    filter(grepl(trans_string, state, fixed = T))
  
  states_undisc <- state_trans %>%
    rowwise() %>%
    do({
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
    ungroup()

  if (nrow(states_undisc) > 0) {
    states_disc <- states_undisc %>%
      mutate(
        value = disc_fun(name),
        name = paste0(".disc_", name)
      )
  } else {
    states_disc <- NULL
  }
  
  rbind(states_undisc, states_disc)
}
parse_hero_summaries <- function(data, values, health, strategies, states) {
  
  # Check that there are no duplicate entries
  duplicates <- group_by(data, name, value) %>%
    mutate(n = seq_len(n())) %>%
    filter(n > 1)
  
  if (nrow(duplicates) > 0) {
    msg <- paste0(
      "Summary '",
      duplicates$name[1],
      "' contains duplicate value '",
      duplicates$value[1],
      "'."
    )
    stop(msg, call. = F)
  }
    
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
  state_summs <- filter(data, value %in% values$name)
  
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
      mutate(
        value = disc_fun(name),
        name = paste0(".disc_", name)
      )
  } else {
    sum_disc <- NULL
  }
  
  rbind(sum_undisc, sum_disc)
}
parse_hero_summaries_start <- function(data, values, strategies) {

  start_summs <- filter(data, value %in% values$name)
  
  sum_undisc <- plyr::ddply(start_summs, "name", function(x) {
    data.frame(
      .model = strategies,
      value = paste(x$value, collapse="+"),
      stringsAsFactors = F
    )
  })
  
  sum_disc <- sum_undisc
  if (nrow(sum_disc) > 0) sum_disc$name <- paste0(".disc_", sum_disc$name)
  rbind(sum_undisc, sum_disc)
}
parse_hero_summaries_st <- function(data, values, health, strategies, states) {
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
  st_summs <- filter(data, value %in% values$name)
  
  sum_undisc <- plyr::ddply(st_summs, "name", function(x) {
    the_values <- filter(
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
      mutate(
        value = disc_fun(name),
        name = paste0(".disc_", name)
      )
  } else {
    sum_disc <- NULL
  }
  rbind(sum_undisc, sum_disc)
}
parse_hero_trans <- function(data, strategies, states) {
  if ("from" %in% colnames(data)) {
    # Markov
    trans_table_lf <- data %>%
      rowwise() %>%
      do({
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
      ungroup()
    
    dupe_trans <- group_by(trans_table_lf, .model, from, to) %>%
      summarise(n = n()) %>%
      filter(n > 1)
    if (nrow(dupe_trans) > 0) {
      dupe_str <- paste(unique(paste0(dupe_trans$from, ' \u2192 ', dupe_trans$to)), collapse = ', ')
      stop(paste0('Error in transitions, duplicate entries for: ', dupe_str), call. = F)
    }
    trans_table <- reshape2::dcast(trans_table_lf, .model + from ~ to, value.var = "prob", fill = 0) %>%
      reshape2::melt(id.vars = c(".model", "from"), value.name = "prob", variable.name = "to") %>%
      mutate(to = as.character(to))
  } else {
    if ("state" %in% colnames(data)) {
      # Custom PSM
      trans_table_lf <- data %>%
        rowwise() %>%
        do({
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
        ungroup() %>%
        mutate(state = factor(state, levels = states))
      
      dupe_trans <- group_by(trans_table_lf, .model, state) %>%
        summarise(n = n()) %>%
        filter(n > 1)
      if (nrow(dupe_trans) > 0) {
        dupe_str <- paste(unique(dupe_trans$state), collapse = ', ')
        stop(paste0('Error in transitions, duplicate entries for: ', dupe_str), call. = F)
      }

      trans_table <- trans_table_lf %>%
        reshape2::dcast(.model ~ state, value.var = "prob", fill = 0) %>%
        reshape2::melt(id.vars = ".model", value.name = "prob", variable.name = "state") %>%
        mutate(state = as.character(state))
    } else {
      # Regular PSM
      trans_table <- rename(data, .model = strategy)
    }
  }
  trans_table
}
parse_hero_states <- function(hvalues, evalues, hsumms, esumms, strategies, states) {
  bad_names <- lapply(states, function(x) !grepl('^[[:alpha:]]+[[:alnum:]\\_]*$', x)) %>%
    as.logical()
  
  if (any(bad_names)) {
    stop(
      paste0(
        'Invalid state names: ',
        paste(states[bad_names], collapse = ', '),
        '. State names must start with letter and contain only letters, numbers, and underscores.'
    ), call. = F)
  }
  
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
    parse_hero_values(hvalues, TRUE, strategies, states),
    parse_hero_values(evalues, FALSE, strategies, states)
  )
  summaries <- rbind(
    parse_hero_summaries(hsumms, values, TRUE, strategies, states),
    parse_hero_summaries(esumms, values, FALSE, strategies, states)
  )
  states_df <- rbind(
    values,
    summaries
  ) %>%
    mutate(
      .model = factor(.model, levels = strategies),
      .state = factor(.state, levels = states),
      name = factor(name, levels = all_value_names)
    ) %>%
    (function(x) {
      if (nrow(x) > 0) {
        x
      } else {
        tibble(
          .model = factor(strategies, levels = strategies),
          .state = factor(states[1], levels = states),
          name = factor(all_value_names[1], levels = all_value_names),
          value = '0'
        )
      }
    }) %>%
    
    reshape2::dcast(.model+.state~name, value.var = "value", fill = 0, drop = F) %>%
    mutate(.model = as.character(.model), .state = as.character(.state))
  
  states_df
}
parse_hero_start <- function(hvalues, evalues, hsumms, esumms, strategies) {
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
    parse_hero_values_start(hvalues, strategies),
    parse_hero_values_start(evalues, strategies)
  )
  summaries <- rbind(
    parse_hero_summaries_start(hsumms, values, strategies),
    parse_hero_summaries_start(esumms, values, strategies)
  )
  if (nrow(values) == 0 && nrow(summaries) == 0) return(NULL)
  start_df <- rbind(
    values,
    summaries
  ) %>%
    mutate(
      .model = factor(.model, levels = strategies),
      name = factor(name, levels = all_value_names)
    ) %>%
    reshape2::dcast(.model~name, value.var = "value", fill = 0, drop = F) %>%
    mutate(.model = as.character(.model))
  
  start_df
}
parse_hero_states_st <- function(hvalues, evalues, hsumms, esumms, strategies, states) {
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
  trans_string <- "\u2192"
  values <- rbind(
    parse_hero_values_st(hvalues, TRUE, strategies),
    parse_hero_values_st(evalues, FALSE, strategies)
  )
  summaries <- rbind(
    parse_hero_summaries_st(hsumms, values, TRUE, strategies, states),
    parse_hero_summaries_st(esumms, values, FALSE, strategies, states)
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
      mutate(
        .model = factor(.model, levels = strategies),
        .transition = factor(.transition, levels = unique(values$.transition)),
        name = factor(name, levels = all_value_names)
      ) %>%
      reshape2::dcast(.model+.transition~name, value.var = "value", fill = 0, drop=F) %>%
      mutate(
        .model = as.character(.model),
        .transition = as.character(.transition)
      )
    split_trans <- strsplit(as.character(st_df$.transition), trans_string)
    st_df <- mutate(
      st_df,
      from = purrr::map_chr(split_trans, ~.[1]),
      to = purrr::map_chr(split_trans, ~.[2])
    )
    
  } else {
    st_df <- NULL
  }
  st_df
}

#' @export
time_in_days <- function(x, days_per_year) {
  switch(
    tolower(x),
    "days" = 1,
    "weeks" = 7,
    "months" = days_per_year / 12,
    "years" = days_per_year
  )
}

hero_extract_summ <- function(res, summ) {
  
  model_res <- res$run_model
  value_res <- as.data.frame(res$run_model, stringsAsFactors=F)
  
  strategies <- unique(value_res$.strategy_names)
  n_strat <- length(strategies)
  
  indices <- expand.grid(referent = seq_len(n_strat), comparator = seq_len(n_strat)) %>%
    filter(referent != comparator)
  value_names <- setdiff(colnames(value_res), ".strategy_names")
  
  ref_res <- value_res[indices$referent, ]
  comp_res <- value_res[indices$comparator, ]
  delta_res <- ref_res
  delta_res[value_names] <- ref_res[value_names] - comp_res[value_names]
  delta_res$.strategy_names <- paste0(ref_res$.strategy_names, " vs. ", comp_res$.strategy_names)
  all_res <- rbind(value_res, delta_res) %>%
    reshape2::melt(id.vars = ".strategy_names") %>%
    mutate(variable = as.character(variable))
  
  summ_unique <- distinct(summ, name, value)
 
  undisc <- inner_join(
    rename(summ_unique,variable = value),
    all_res,
    by = "variable"
  ) %>%
    mutate(
      outcome = name,
      series = .strategy_names,
      group = variable,
      disc = F
    ) %>%
    select(outcome, series, group, disc, value)
  
  disc <- inner_join(
    mutate(
      summ_unique,
      variable1 = paste0(".disc_", value),
      variable = value
    ) %>%
      select(-value),
    all_res,
    by = c("variable1" = "variable")
  ) %>%
    mutate(
      outcome = name,
      series = .strategy_names,
      group = variable,
      disc = T
    ) %>%
    select(outcome, series, group, disc, value)
  rbind(disc, undisc)
}
hero_extract_nmb <- function(hsumm_res, esumm_res, hsumms) {
  
  unique_hsumms <- distinct(hsumms, name, .keep_all = T) %>%
    select(name, wtp)
  
  nmb_hsumm_res <- filter(hsumm_res, disc, grepl(" vs. ", series, fixed=T)) %>%
    left_join(unique_hsumms, by = c("outcome" = "name")) %>%
    mutate(nmb = value * as.numeric(wtp), type = "health") %>%
    mutate(value = nmb) %>%
    select(outcome, series, group, disc, type, value)
  
  nmb_esumm_res <- filter(esumm_res, disc, grepl(" vs. ", series, fixed=T)) %>%
    mutate(value = -value, type = "economic") %>%
    select(outcome, series, group, disc, type, value)
  
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
    plyr::ddply(c("hsumm","esumm"), function(x){
      temp_res <- res
      class(temp_res) <- "list"
      ce <- list(
        .effect = as.lazy(x$hsumm,  res$ce$.effect$env),
        .cost = as.lazy(x$esumm,  res$ce$.cost$env)
      )
      temp_res$run_model <- temp_res$run_model %>%
        mutate(!!!lazy_eval(as.lazy_dots(ce), data = .)) %>%
        arrange(.cost, desc(.effect))
      ordering <- order(temp_res$run_model$.cost, temp_res$run_model$.effect)
      class(temp_res) <- c("run_model", "data.frame")
      summary(temp_res, strategy_order = ordering)$res_comp %>%
        transmute(
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
hero_extract_ce_pw <- function(res, hsumm_res, esumm_res) {
  
  abs_hsumm_res <- filter(hsumm_res, disc, !grepl(" vs. ", series, fixed=T)) %>%
    mutate(comparator = series) %>%
    select(outcome, comparator, group, disc, value) %>%
    group_by(outcome, comparator) %>%
    summarize(effect = sum(value)) 
  
  abs_esumm_res <- filter(esumm_res, disc, !grepl(" vs. ", series, fixed=T)) %>%
    mutate(comparator = series) %>%
    select(outcome, comparator, group, disc, value) %>%
    group_by(outcome, comparator) %>%
    summarize(cost = sum(value)) 
  
  
  delta_hsumm_res <- filter(hsumm_res, disc, grepl(" vs. ", series, fixed=T)) %>%
    select(outcome, series, group, disc, value) %>%
    group_by(outcome, series) %>%
    summarize(deffect = sum(value)) %>%
    mutate(
      referent = purrr::map_chr(
        strsplit(series, ' vs. '),
        ~.[1]
      ),
      comparator = purrr::map_chr(
        strsplit(series, ' vs. '),
        ~.[2]
      )
    )
  
  delta_esumm_res <- filter(esumm_res, disc, grepl(" vs. ", series, fixed=T)) %>%
    select(outcome, series, group, disc, value) %>%
    group_by(outcome, series) %>%
    summarize(dcost = sum(value)) %>%
    mutate(
      referent = purrr::map_chr(
        strsplit(series, ' vs. '),
        ~.[1]
      ),
      comparator = purrr::map_chr(
        strsplit(series, ' vs. '),
        ~.[2]
      )
    )
  
  outcome_tbl <- expand.grid(
    health = unique(hsumm_res$outcome),
    econ = unique(esumm_res$outcome),
    stringsAsFactors = F
  ) %>%
    left_join(delta_hsumm_res, by = c('health' = 'outcome')) %>%
    left_join(delta_esumm_res, by = c('econ' = 'outcome', 'series' = 'series', 'referent' = 'referent', 'comparator' = 'comparator')) %>%
    left_join(abs_hsumm_res, by = c('health' = 'outcome', 'comparator' = 'comparator')) %>%
    left_join(abs_esumm_res, by = c('econ' = 'outcome', 'comparator' = 'comparator')) %>%
    mutate(
      icer = compute_pw_icer(deffect, dcost),
      icer_string = format_icer(icer)
    ) %>%
    select(health, econ, series, referent, comparator, effect, cost, deffect, dcost, icer, icer_string)
  
  outcome_tbl
}
hero_extract_trace <- function(res, corrected = F) {
  if(!is.null(res$oldmodel)) {
    params <- res$oldmodel$eval_strategy_list[[1]]$parameters
  } else {
    params <- res$eval_strategy_list[[1]]$parameters
  }
  
  time <- rbind(
    if(!corrected) data.frame(model_day=0,model_week=0,model_month=0,model_year=0) else data.frame(),
    distinct(
      params,
      model_day,
      model_week,
      model_month,
      model_year
    )
  )
  trace <- plyr::ldply(
    res$eval_strategy_list,
    function(x) {
      if(!corrected) x$counts_uncorrected
      else x$counts
    }
  ) %>%
    rename(
      series = .id
    )
  cbind(time, trace)
}

hero_extract_psa_summ <- function(res, summ) {
  
  all_res <- rbind(res) %>%
    reshape2::melt(id.vars = c(".strategy_names", ".index")) %>%
    mutate(variable = as.character(variable))
  
  summ_unique <- distinct(summ, name, value)
  
  undisc <- inner_join(
    rename(summ_unique,variable = value),
    all_res,
    by = "variable"
  ) %>%
    mutate(
      sim = .index,
      outcome = name,
      series = .strategy_names,
      group = variable,
      disc = F
    ) %>%
    select(outcome, series, sim, group, disc, value)
  
  undisc_summ <- undisc %>% group_by(outcome, series, disc, sim) %>% summarize(value = sum(value))
  
  disc <- inner_join(
    mutate(
      summ_unique,
      variable1 = paste0(".disc_", value),
      variable = value
    ) %>%
      select(-value),
    all_res,
    by = c("variable1" = "variable")
  ) %>%
    mutate(
      sim = .index,
      outcome = name,
      series = .strategy_names,
      group = variable,
      disc = T
    ) %>%
    select(outcome, series, sim, group, disc, value)
  
  disc_summ <- disc %>% group_by(outcome, series, disc, sim) %>% summarize(value = sum(value))
  
  all_abs <- rbind(
    disc_summ,
    rename(select(disc, group, series, disc, sim, value), outcome = group),
    undisc_summ,
    rename(select(undisc, group, series, disc, sim, value), outcome = group)
  )
  
  strat_names <- unique(all_abs$series)
  
  comparisons <- crossing(ref = strat_names, comp = strat_names) %>%
    filter(ref != comp) %>%
    left_join(
      transmute(all_abs, ref = series, disc, outcome, sim, ref_value = value),
      by = 'ref'
    ) %>%
    left_join(
      transmute(all_abs, comp = series, disc, outcome, sim, comp_value = value),
      by = c('comp', 'disc', 'outcome', 'sim')
    ) %>%
    mutate(value = ref_value - comp_value, series = paste0(ref, ' vs. ', comp)) %>%
    select('series', 'disc', 'outcome', 'sim', 'value')
  
  rbind(all_abs, comparisons)
}
hero_extract_psa_ceac <- function(res, hsumms, esumms, wtps) {
  unique_hsumms <- paste0(".disc_", unique(hsumms$name))
  unique_esumms <- paste0(".disc_", unique(esumms$name))
  do_ceacs <- function(x) {
    do_one_ceac <- function(y) {
      ceac_res <- res
      ceac_res$.effect <- res[[y$hsumm]]
      ceac_res$.cost <- res[[y$esumm]]
      acceptability_curve(ceac_res, wtps)
    }
    if (nrow(x) == 1) {
      res_df <- do_one_ceac(x)
      res_df$hsumm <- x$hsumm
      res_df$esumm <- x$esumm
    } else {
      res_df <- plyr::ddply(x, c("hsumm","esumm"), do_one_ceac)
    }
    res_df
  }
  expand.grid(
    hsumm = unique_hsumms,
    esumm = unique_esumms,
    stringsAsFactors = F
  ) %>%
    do_ceacs() %>%
    reshape2::dcast(hsumm+esumm+.ceac~.strategy_names, value.var = ".p") %>%
    rename(health_outcome = hsumm, econ_outcome = esumm, wtp = .ceac)
}
hero_extract_psa_evpi <- function(res, hsumms, esumms, step, max) {
  unique_hsumms <- paste0(".disc_", unique(hsumms$name))
  unique_esumms <- paste0(".disc_", unique(esumms$name))
  expand.grid(
    hsumm = unique_hsumms,
    esumm = unique_esumms,
    stringsAsFactors = F
  ) %>%
    plyr::ddply(c("hsumm","esumm"), function(x) {
      res$psa$.effect <- res$psa[[x$hsumm]]
      res$psa$.cost <- res$psa[[x$esumm]]
      compute_evpi(res, seq(from = 0, to = max, by = step))
    }) %>%
    rename(health_outcome = hsumm, econ_outcome = esumm, wtp = .ceac, value = .evpi)
}
hero_extract_psa_scatter <- function(res, hsumms, esumms) {
  hsumms_df <- distinct(hsumms, name, .keep_all = T) %>%
    select(name, wtp) %>%
    mutate(name = paste0(".disc_", name))
  unique_hsumms <- hsumms_df$name
  unique_esumms <- paste0(".disc_", unique(esumms$name))
  abs_res <- expand.grid(
    hsumm = unique_hsumms,
    esumm = unique_esumms,
    stringsAsFactors = F
  ) %>%
    left_join(hsumms_df, by = c("hsumm" = "name")) %>%
    plyr::ddply(c("hsumm","esumm"), function(x) {
      the_wtp <- x$wtp
      data.frame(
        series = res$.strategy_names,
        sim = res$.index,
        x = res[[x$hsumm]],
        y = res[[x$esumm]],
        wtp = the_wtp,
        stringsAsFactors = F
      )
    }) %>%
    rename(
      health_outcome = hsumm,
      econ_outcome = esumm
    )
  
  abs_res
}

build_hero_model <- function(...) {
  
  # Capture arguments
  dots <- list(...)
  
  # Check strategies
  bad_names <- lapply(dots$strategies$name, function(x) !grepl('^[[:alpha:]]+[[:alnum:]\\_]*$', x)) %>%
    as.logical()
  
  if (any(bad_names)) {
    stop(
      paste0(
        'Invalid strategy names: ',
        paste(dots$strategies$name[bad_names], collapse = ', '),
        '. Strategy names must start with letter and contain only letters, numbers, and underscores.'
      ), call. = F)
  }
  
  if (nrow(dots$strategies) < 2) {
    stop('You must have at least two strategies selected to run a model.', call. = F)
  }
  
  if (is.null(dots$psa)) {
    dots$psa <- list(
      parallel = F
    )
    dots$psa$n = 1
  }
  
  settings <- parse_hero_settings(dots$settings)
  
  # Format parameters Table
  params <- parse_hero_vars(
    dots$variables,
    settings,
    dots$groups
  )
  
  # Format survival distributions table
  surv <- parse_hero_obj_vars(dots$surv_dists)
  
  # Format groups table
  groups_tbl <- parse_hero_groups(dots$groups)
  
  # Format transitions table
  trans <- parse_hero_trans(dots$transitions, dots$strategies$name, dots$states$name)
  
  # Format state list
  state_list <- parse_hero_states(
    dots$hvalues,
    dots$evalues,
    dots$hsumms,
    dots$esumms,
    dots$strategies$name,
    dots$states$name
  )
  
  # Format state transitions list
  st_list <- parse_hero_states_st(
    dots$hvalues,
    dots$evalues,
    dots$hsumms,
    dots$esumms,
    dots$strategies$name,
    dots$states$name
  )
  
  # Format startin list
  start_list <- parse_hero_start(
    dots$hvalues,
    dots$evalues,
    dots$hsumms,
    dots$esumms,
    dots$strategies$name
  )
  
  # Format state time limits
  limits <- as.numeric(dots$states$limit)
  names(limits) <- dots$states$name
  limits <- limits[!is.na(limits) & !(limits == 0)]
  
  if (!is.null(dots$cores)) cores <- dots$cores
  else cores <- cores_to_use()

  # Fix column names
  dots$tables <- lapply(dots$tables, function(x) {
    if(class(x) == 'list') return(x)
    colnames(x) <- gsub("[\r\n]", "", colnames(x))
    x
  })
  psa_n <- '1000'
  if (!is.null(dots$psa$n)) {
    psa_n <- as.character(dots$psa$n)
  }
  
  # Return model object
  list(
    states = state_list,
    st = st_list,
    start = start_list,
    tm = trans,
    param = params,
    options = tibble::tribble(
      ~option,  ~value,
      "cost",   paste0(".disc_", dots$esumms$name[1]),
      "effect", paste0(".disc_", dots$hsumms$name[1]),
      "method", settings$method,
      "disc_method", settings$disc_method,
      "cycles", as.character(max(1, round(settings$n_cycles,0))),
      "n",      psa_n,
      "init",   paste(dots$states$prob,collapse = ", "),
      "num_cores", as.character(cores)
    ),
    data = dots$tables,
    state_time_limit = limits,
    source = dots$scripts,
    aux_params = surv,
    psa = dots$psa,
    report_progress = dots$create_progress_reporter(), # For main process
    create_progress_reporter = dots$create_progress_reporter, # For child processes
    individual_level = T
  )
}

check_dsa_vars <- function(x) {
  empty_low <- x$low == '' | is.na(x$low)
  empty_high <- x$high == '' | is.na(x$high)
  all_missing <- all(empty_low) || all(empty_high)
  if (all_missing) {
    err_msg <- 'You must vary at least one parameter in DSA Inputs in order to run DSA.'
    stop(err_msg, call. = F)
  }
  missing_low_or_high <- empty_low != empty_high
  if (any(missing_low_or_high)) {
    err_msg <- paste0('Parameter')
    stop(
      paste0(
        'Parameters varied in DSA must have both low and high values defined in DSA Inputs. The following parameters were missing low/high values: ',
        paste('"', x$name[missing_low_or_high], '"', sep =, collapse = ", ")
      ),
      call. = F
    )
  }
}

#' @export
run_hero_psa <- function(...) {
  # Capture arguments
  dots <- patch_progress_funcs(list(...))
  n_groups <- max(1, nrow(as.data.frame(dots$groups)))
  n_strats <- nrow(dots$strategies)
  n_sims <- n_strats * n_groups * dots$psa$n
  
  max_prog <- get_psa_max_progress(dots)
  try(dots$report_max_progress(max_prog))
  try(dots$report_progress(1L))
  if(nrow(as.data.frame(dots$groups)) <= 1) {
    # Homogenous model
    # Compile model object
    args <- do.call(build_hero_model, dots)
    args$run_psa <- T
    
    # Run Model
    set.seed(dots$psa$seed)
    psa_model <- do.call(run_model_api, args)
    psa_res_df <- psa_model$psa$psa
  } else {
    # Heterogeneous model
    # Run PSA analysis for each group
    group_vars <- setdiff(colnames(dots$groups), c("name", ".group_weight"))
    psas <- plyr::alply(dots$groups, 1, function(x) {
      
      # Run model for given group
      group_args <- dots
      group_args$groups <- x
      set.seed(dots$psa$seed)
      args <- do.call(build_hero_model, group_args)
      args$run_psa <- T
      do.call(run_model_api, args)
    })
    psa_model <- psas[[1]]
    psa_res_df <- plyr::ldply(psas, function(x) x$psa$psa)
    psa_res_df <- psa_res_df[ , setdiff(colnames(psa_res_df), group_vars)]
    col_indices <- setdiff(colnames(psa_res_df), c(".strategy_names", ".index", "name", "weight", ".group_weight"))

    psa_res_df <- psa_res_df %>%
      group_by(.strategy_names, .index) %>%
      group_split() %>%
      map(function(iter_res) {
        mutate(
          iter_res[ ,col_indices] * iter_res$.group_weight / sum(iter_res$.group_weight),
          .strategy_names = iter_res$.strategy_names,
          .index = iter_res$.index
        )
      }) %>%
      bind_rows() %>%
      group_by(.strategy_names, .index) %>%
      summarize_all(sum) %>%
      ungroup()
  }
  
  thresh_max <- dots$psa$thresh_max
  thresh_step <- dots$psa$thresh_step
  thresh_n_steps <- thresh_max / thresh_step
  
  if (!is.null(dots$results_so_far)) {
    psa_res_df$.index <- psa_res_df$.index + max(dots$results_so_far$.index, na.rm = T)
    psa_res_df <- rbind(dots$results_so_far, psa_res_df)
  }
  
  if(is.null(dots$interim) || !dots$interim) {
  
    scatter <- hero_extract_psa_scatter(psa_res_df, dots$hsumms, dots$esumms)
    outcomes <- hero_extract_psa_summ(psa_res_df, dots$hsumms)
    outcomes_summary <- outcomes %>%
      group_by(series, outcome, disc) %>%
      summarize(
        mean = mean(value),
        sd = sd(value),
        n = n(),
        lcl95 = unname(quantile(value, 0.025)),
        ucl95 = unname(quantile(value, 0.975)),
        min = min(value),
        lowerq = unname(quantile(value, 0.25)),
        median = median(value),
        upperq = unname(quantile(value, 0.75)),
        max = max(value)
      ) %>%
      ungroup() %>%
      reshape2::melt(id.vars = c("series", "outcome", "disc"), variable.name = "statistic", value.name = "value") %>%
      reshape2::dcast(outcome+series+disc~statistic, value.var = "value") %>%
      as_tibble()
    costs <- hero_extract_psa_summ(psa_res_df, dots$esumms)
    costs_summary <- costs %>%
      group_by(series, outcome, disc) %>%
      summarize(
        mean = mean(value),
        sd = sd(value),
        n = n(),
        lcl95 = unname(quantile(value, 0.025)),
        ucl95 = unname(quantile(value, 0.975)),
        min = min(value),
        lowerq = unname(quantile(value, 0.25)),
        median = median(value),
        upperq = unname(quantile(value, 0.75)),
        max = max(value)
      ) %>%
      ungroup() %>%
      reshape2::melt(id.vars = c("series", "outcome", "disc"), variable.name = "statistic", value.name = "value") %>%
      reshape2::dcast(outcome+series+disc~statistic, value.var = "value") %>%
      as_tibble()
    ceac <- hero_extract_psa_ceac(psa_res_df, dots$hsumms, dots$esumms, seq(from = 0,to = dots$psa$thresh_max,by = thresh_step))
    temp_model <- psa_model$psa
    temp_model$psa <- psa_res_df
    evpi <- hero_extract_psa_evpi(temp_model, dots$hsumms, dots$esumms, thresh_step, dots$psa$thresh_max)
    
    scatter_compressed <- scatter %>%
      group_by(health_outcome, econ_outcome, series) %>%
      group_split() %>%
      purrr::map(function(x) {
        list(
          health_outcome = substring(x$health_outcome[1], 7),
          econ_outcome = substring(x$econ_outcome[1], 7),
          series = x$series[1],
          data = mutate(
            select(x, -health_outcome, -econ_outcome, -series)
          )
        )
      })
    
    try(dots$report_progress(1L))
    list(
      api_ver = '2.0',
      scatter = scatter_compressed,
      outcomes_summary = outcomes_summary,
      costs_summary = costs_summary,
      ceac = ceac,
      evpi = evpi
    )
  } else {
    
    list(
      results = psa_res_df
    )
  }
}

#' @export
run_markdown <- function(...) {
  dots <- patch_progress_funcs(list(...))
  max_prog <- get_code_preview_max_progress(dots)
  try(dots$report_max_progress(max_prog))
  text <- dots$text
  data <- dots$data
  eval_env <- new.env(parent = parent.frame())
  try(dots$report_progress(1L))
  if(!is.null(data)) {
    plyr::l_ply(
      seq_len(length(data)),
      function(i) assign(names(data)[i], data[[i]], envir = eval_env)
    )
  }
  try(dots$report_progress(1L))
  r_filename <- paste0(dots$name, ".r")
  md_filename <- paste0(dots$name, ".md")
  html_filename <- paste0(dots$name, ".html")
  writeLines(text, con = paste0(dots$name, ".r"))
  try(dots$report_progress(1L))
  knitr::spin(r_filename, knit = T, envir = eval_env, precious = F, doc = '^##\\s*')
  try(dots$report_progress(1L))
  file.remove(md_filename)
  file.remove(r_filename)
  try(dots$report_progress(1L))
  if (!is.null(dots$.manifest)) {
    dots$.manifest$register_file('html_output', html_filename, 'Code Editor Preview HTML Output', default = T)
  }
  list(
    vars = ls(eval_env)
  )
}

#' @export
package_hero_model <- function(...) {
  dots <- patch_progress_funcs(list(...))

  max_prog <- get_r_project_max_progress(dots)
  try(dots$report_max_progress(max_prog))
  
  model_object <- list(...)
  model_object$report_progress <- NULL
  model_object$report_max_progress <- NULL
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
  filename <- paste0(dots$name, ".zip")
  write(rproj_string, paste0(dots$name, ".rproj"))
  try(dots$report_progress(1L))
  write(rcode_string, "run.R")
  try(dots$report_progress(1L))
  saveRDS(model_object, "model.rds")
  try(dots$report_progress(1L))
  utils::zip(
    filename,
    c(paste0(dots$name, ".rproj"), "run.R", "model.rds"),
    flags="-q"
  )
  try(dots$report_progress(1L))
  if(!is.null(dots$.manifest)) {
     dots$.manifest$register_file('r_model_export', filename, 'R Export', default = T)
  }
  file.remove(paste0(dots$name, ".rproj"))
  file.remove("run.R")
  file.remove("model.rds")
  try(dots$report_progress(1L))
  list(success = T)
}

cores_to_use <- function() {
  max(1, round((parallel::detectCores() - 2)/3, 0))
}