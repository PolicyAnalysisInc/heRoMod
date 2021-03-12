#' @export
run_analysis <- function(...) {
  data <- list(...)
  manifest <- create_manifest()
  data$.manifest <- manifest
  runner <- switch(
    data$analysis,
    'psa' = run_hero_psa,
    'dsa' = run_hero_dsa,
    'vbp' = run_hero_vbp,
    'bc' = run_hero_bc,
    'scen' = run_hero_scen,
    'excel' = export_hero_xlsx,
    'code_preview' = run_code_preview_compat,
    'r_project' = package_hero_model,
    stop('Parameter "analysis" must be one of: "bc", "vbp", "dsa", "psa", "scen", "excel", "code_preview", "r_project".')
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
  jsonlite::write_json(res, filename)
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
      rename(!!!syms(c(".group" = "name")))
    group_vars <- groups %>%
      colnames() %>%
      setdiff(".weights") %>%
      plyr::ldply(function(name) data.frame(parameter = name, value = groups[[name]][1], low =  NA, high = NA, psa = NA))
  } else {
    group_vars <- NULL
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
    mutate(group = .group, .weights = as.numeric(.weights))
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
  rbind(disc, undisc)
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

compile_unit_values <- function(x) {
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
        mutate(state = paste0(.from_name_expanded, "\u2192", .to_name_expanded)) %>%
        data.table::data.table() %>%
        data.table::dcast(strategy+state+markov_cycle~variable, value.var = "value")
    } else {
      trans_df <- NULL
    }
    out_df <- rbind(data.table::rbindlist(states_list), trans_df) %>%
      arrange(strategy, state, markov_cycle) %>%
      rename(cycle = markov_cycle)
    
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
        mutate(state = paste0(.from_name_expanded, "\u2192", .to_name_expanded)) %>%
        data.table::data.table() %>%
        data.table::dcast(strategy+group+state+markov_cycle~variable, value.var = "value")
    } else {
      trans_df <- NULL
    }
    out_df <- rbind(data.table::rbindlist(states_list), trans_df) %>%
      arrange(strategy, group, state, markov_cycle) %>%
      rename(cycle = markov_cycle)
    
      out_df[ ,c("strategy", "group", "state", "cycle", value_names), drop = F]
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
    
    value_list <- lapply(seq_len(n_strategy), function(i) {
      the_df <- x$model_runs$eval_strategy_list[[i]]$values
      the_df$strategy <- strategy_names[i]
      the_df
    })
    
    ret <- bind_rows(value_list) %>%
      arrange(strategy, markov_cycle) %>%
      rename(cycle = markov_cycle)
    
    ret[ ,c("strategy", "cycle", value_names), drop = F]
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
    
    value_list <- lapply(seq_len(n_strategy), function(i) {
      lapply(seq_len(n_group), function(j) {
        the_df <- x$model_runs$eval_strategy_list[[i]]$values
        the_df$strategy <- strategy_names[i]
        the_df$group <- group_names[j]
        the_df
      })
    })
    ret <- value_list %>%
      unlist(recursive=F) %>%
      unname() %>%
      bind_rows() %>%
      arrange(strategy, group, markov_cycle) %>%
      rename(cycle = markov_cycle)
    
    ret[ ,c("strategy", "group", "cycle", value_names), drop = F]
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
  
  # Return model object
  list(
    states = state_list,
    st = st_list,
    start = start_list,
    tm = trans,
    param = params,
    demo = groups_tbl,
    options = tibble::tribble(
      ~option,  ~value,
      "cost",   paste0(".disc_", dots$esumms$name[1]),
      "effect", paste0(".disc_", dots$hsumms$name[1]),
      "method", settings$method,
      "disc_method", settings$disc_method,
      "cycles", as.character(max(1, round(settings$n_cycles,0))),
      "n",      as.character(dots$psa$n),
      "init",   paste(dots$states$prob,collapse = ", "),
      "num_cores", as.character(cores)
    ),
    data = dots$tables,
    state_time_limit = limits,
    source = dots$scripts,
    aux_params = surv,
    psa = dots$psa,
    report_progress = dots$report_progress
  )
}

#' @export
run_hero_bc <- function(...) {
  
  # Capture arguments
  dots <- patch_progress_funcs(list(...))
  
  # Compile model object
  args <- do.call(build_hero_model, dots)
  args$run_demo = !(is.null(args$demo))
  
  max_prog <- get_bc_max_progress(dots)
  try(dots$report_max_progress(max_prog))
  
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
  pw_ce_res <- hero_extract_ce_pw(main_res, health_res, econ_res)
  trace_res <- hero_extract_trace(main_res)
  
  # Return
  list(
    trace = trace_res,
    outcomes = health_res,
    costs = econ_res,
    ce = ce_res,
    pairwise_ce = pw_ce_res,
    nmb = nmb_res,
    api_ver = '2.0'
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
    group_vars <- setdiff(colnames(dots$groups), c("name", "weight"))
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
    col_indices <- setdiff(colnames(psa_res_df), c(".strategy_names", ".index", "name", "weight"))
    psa_res_df$weight <- as.numeric(psa_res_df$weight)
    psa_res_df[ , col_indices] <- psa_res_df[ , col_indices] * psa_res_df$weight
    psa_res_df <- psa_res_df %>%
      select(-name) %>%
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
      group_by(series, group) %>%
      summarize(
        mean = mean(value),
        sd = sd(value),
        min = min(value),
        lowerq = unname(quantile(value, 0.25)),
        median = median(value),
        upperq = unname(quantile(value, 0.75)),
        max = max(value)
      ) %>%
      ungroup() %>%
      reshape2::melt(id.vars = c("series", "group"), variable.name = "statistic", value.name = "value") %>%
      reshape2::dcast(group+series~statistic, value.var = "value")
    costs <- hero_extract_psa_summ(psa_res_df, dots$esumms)
    costs_summary <- costs %>%
      group_by(series, group) %>%
      summarize(
        mean = mean(value),
        sd = sd(value),
        min = min(value),
        lowerq = unname(quantile(value, 0.25)),
        median = median(value),
        upperq = unname(quantile(value, 0.75)),
        max = max(value)
      ) %>%
      ungroup() %>%
      reshape2::melt(id.vars = c("series", "group"), variable.name = "statistic", value.name = "value") %>%
      reshape2::dcast(group+series~statistic, value.var = "value")
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
export_hero_xlsx <- function(...) {
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
  writeWorkbook(lapply(wb_list, sanitize_df), filename)
  if (!is.null(dots$.manifest)) {
    dots$.manifest$register_file('excel_output', filename, 'Export to excel output', default = T)
  }
  ret <- wb_list
  
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

cores_to_use <- function() {
  max(1, round((parallel::detectCores() - 2)/3, 0))
}