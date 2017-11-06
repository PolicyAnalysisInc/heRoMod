
parse_hero_vars <- function(data, clength) {
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
    "state_year",          "state_time * cycle_length_years"
  )
  user_pars <- dplyr::transmute(
    data,
    parameter = name,
    value = value
  )
  rbind(
    hero_pars,
    user_pars
  )
}
parse_hero_values <- function(data, disc, strategies, clength) {
  disc_adj <- rescale_discount_rate(disc, 365, clength)
  disc_fun <- function(x) paste0("discount(", x, ",", disc_adj, ")")
  
  states_undisc <- data %>%
    dplyr::rowwise() %>%
    dplyr::do({
      if(.$strategy == "All") {
        data.frame(
          name = .$name,
          .model = strategies,
          .state = .$state,
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
    }) %>%
    dplyr::ungroup()
  
  states_disc <- states_undisc %>%
    dplyr::mutate(
      value = disc_fun(name),
      name = paste0(".disc_", name)
    )
  
  rbind(states_undisc, states_disc)
}
parse_hero_summaries <- function(data, values, disc, strategies, states, clength) {
  
  disc_adj <- rescale_discount_rate(disc, 365, clength)
  disc_fun <- function(x) paste0("discount(", x, ",", disc_adj, ")")
  
  sum_undisc <- plyr::ddply(data, "name", function(x) {
    data.frame(
      .model = rep(strategies, length(states)),
      .state = rep(states, each = length(strategies)),
      value = paste(x$value, collapse="+"),
      stringsAsFactors = F
    )
  })
  sum_disc <- sum_undisc %>%
    dplyr::mutate(
      value = disc_fun(name),
      name = paste0(".disc_", name)
    )
  
  rbind(sum_undisc, sum_disc)
}
parse_hero_trans <- function(data, strategies) {
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
}
parse_hero_states <- function(hvalues, evalues, hsumms, esumms, hdisc, edisc, strategies, states, clength) {
  values <- rbind(
    parse_hero_values(hvalues, hdisc, strategies, clength),
    parse_hero_values(evalues, edisc, strategies, clength)
  )
  summaries <- rbind(
    parse_hero_summaries(hsumms, values, hdisc, strategies, states, clength),
    parse_hero_summaries(esumms, values, edisc, strategies, states, clength)
  )
  states_df <- rbind(
    values,
    summaries
  ) %>%
    dplyr::mutate(
      .model = factor(.model, levels = strategies),
      .state = factor(.state, levels = states),
      name = factor(name, levels = unique(name))
    ) %>%
    reshape2::dcast(.model+.state~name, value.var = "value", fill = 0)
  
  states_df
}
hero_extract_summ <- function(res, summ, delta = F, referent = NULL) {
  
  summ_unique <- dplyr::distinct(summ, name, value)
  if(delta) {
    model_res <- res$model_runs
    class(model_res) <- "list"
    model_res$central_strategy <- referent
    class(model_res) <- c("run_model", "data.frame")
    model_res <- scale.run_model(model_res, center = T)
    value_res <- as.data.frame(res$model_runs$run_model, stringsAsFactors=F) %>%
      reshape2::melt(id.vars = ".strategy_names") %>%
      dplyr::mutate(variable = as.character(variable)) %>%
      dplyr::filter(.strategy_names != referent)
  } else {
    model_res <- res$model_runs$run_model
    value_res <- as.data.frame(res$model_runs$run_model, stringsAsFactors=F) %>%
      reshape2::melt(id.vars = ".strategy_names") %>%
      dplyr::mutate(variable = as.character(variable))
  }
  
  undisc <- dplyr::inner_join(
    dplyr::rename(summ_unique,variable = value),
    value_res,
    by = "variable"
  ) %>%
    dplyr::mutate(
      outcome = name,
      series = if(delta) paste0(referent, " vs. ", .strategy_names) else .strategy_names,
      group = variable,
      disc = F,
      value = if(delta) -value else value
    ) %>%
    dplyr::select(outcome, series, group, disc, value)
  
  disc <- dplyr::inner_join(
    dplyr::mutate(
      summ_unique,
      variable1 = paste0(".disc_", value),
      variable = value
    ) %>%
      dplyr::select(-value),
    value_res,
    by = c("variable1" = "variable")
  ) %>%
    dplyr::mutate(
      outcome = name,
      series = if(delta) paste0(referent, " vs. ", .strategy_names) else .strategy_names,
      group = variable,
      disc = T,
      value = if(delta) -value else value
    ) %>%
    dplyr::select(outcome, series, group, disc, value)
  rbind(disc, undisc)
}
hero_extract_ce <- function(res) {
  summary(res$model_runs)$res_comp %>%
    dplyr::transmute(
      name = .strategy_names,
      cost = .cost,
      effect = .effect,
      dcost = .dcost,
      deffect = .deffect,
      dref = .dref,
      icer = .icer
    )
}
hero_extract_trace <- function(res) {
  time <- rbind(
    data.frame(model_day=0,model_week=0,model_month=0,model_year=0),
    dplyr::distinct(
      res$model_runs$eval_strategy_list[[1]]$parameters,
      model_day,
      model_week,
      model_month,
      model_year
    )
  )
  trace <- ldply(
    res$model_runs$eval_strategy_list,
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
run_hero_model <- function(model, cost, effect, type = "base case") {
  params <- parse_hero_vars(model$variables, model$settings$cycle_length)
  trans <- parse_hero_trans(model$transitions, model$strategies$name)
  states <- parse_hero_states(
    model$hvalues,
    model$evalues,
    model$hsumms,
    model$esumms,
    model$settings$disc_eff,
    model$settings$disc_cost,
    model$strategies$name,
    model$states$name,
    model$settings$cycle_length
  )
  tables <- model$tables
  limits <- model$states$limit
  names(limits) <- model$states$name
  limits <- limits[!is.na(limits)]
  heemod_res <- run_model_api(
    states = states,
    tm = trans,
    param = params,
    options = tibble::tribble(
      ~option,  ~value,
      "cost",   paste0(".disc_", cost),
      "effect", paste0(".disc_", effect),
      "method", "beginning",
      "cycles", model$settings$n_cycles,
      "n",      1,
      "init",   "1, 0, 0"
    ),
    data = tables,
    run_dsa = F,
    run_psa = F,
    run_demo = F,
    state_time_limit = limits
  )
  
  health_res <- rbind(
    hero_extract_summ(heemod_res, model$hsumms),
    plyr::ldply(model$strategies$name, function(x) {
      hero_extract_summ(
        heemod_res,
        model$hsumms,
        delta = T,
        referent = x
      )
    })
  )
  econ_res <- rbind(
    hero_extract_summ(heemod_res, model$esumms),
    plyr::ldply(model$strategies$name, function(x) {
      hero_extract_summ(
        heemod_res,
        model$esumms,
        delta = T,
        referent = x
      )
    })
  )
  ce_res <- hero_extract_ce(heemod_res)
  
  trace_res <- hero_extract_trace(heemod_res)
  
  list(
    trace = trace_res,
    outcomes = health_res,
    costs = econ_res,
    ce = ce_res
  )
  
}


#' @export
run_markdown <- function(text) {
  eval_env <- new.env(parent = parent.frame())
  writeLines(text, con = 'output.r')
  knitr::spin('output.r', knit = T, envir = eval_env, precious = F, doc = '^##\\s*')
  ls(eval_env)
}

