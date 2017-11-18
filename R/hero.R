
parse_hero_vars <- function(data, clength, hdisc, edisc) {
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
parse_hero_values <- function(data, health, strategies, clength) {
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
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
parse_hero_summaries <- function(data, values, health, strategies, states, clength) {
  if(health) {
    disc_var <- "disc_h"
  } else {
    disc_var <- "disc_e"
  }
  disc_fun <- function(x) paste0(x, " * ", disc_var)
  
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
parse_hero_states <- function(hvalues, evalues, hsumms, esumms, strategies, states, clength) {
  values <- rbind(
    parse_hero_values(hvalues, TRUE, strategies, clength),
    parse_hero_values(evalues, FALSE, strategies, clength)
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
      name = factor(name, levels = unique(name))
    ) %>%
    reshape2::dcast(.model+.state~name, value.var = "value", fill = 0)
  
  states_df
}
hero_extract_summ <- function(res, summ) {
  
  
  model_res <- res$model_runs$run_model
  value_res <- as.data.frame(res$model_runs$run_model, stringsAsFactors=F)
  
  strategies <- unique(value_res$.strategy_names)
  n_strat <- length(strategies)
  
  indices <- expand.grid(referent = seq_len(n_strat), comparator = seq_len(n_strat))
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
run_hero_model <- function(decision, settings, strategies, states, transitions,
                           hvalues, evalues, hsumms, esumms, variables,
                           tables, scripts, cost, effect, type = "base case") {
  params <- parse_hero_vars(variables, settings$cycle_length, settings$disc_eff, settings$disc_cost)
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
  tables <- tables
  limits <- states$limit
  names(limits) <- states$name
  limits <- limits[!is.na(limits) & !(limits == 0)]
  heemod_res <- run_model_api(
    states = state_list,
    tm = trans,
    param = params,
    options = tibble::tribble(
      ~option,  ~value,
      "cost",   paste0(".disc_", cost),
      "effect", paste0(".disc_", effect),
      "method", "beginning",
      "cycles", settings$n_cycles,
      "n",      1,
      "init",   paste(states$prob,collapse=", ")
    ),
    data = tables,
    run_dsa = F,
    run_psa = F,
    run_demo = F,
    state_time_limit = limits,
    source = scripts
  )
  
  health_res <- hero_extract_summ(heemod_res, hsumms)
  econ_res <- hero_extract_summ(heemod_res, esumms)
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
run_markdown <- function(text, tables = NULL) {
  eval_env <- new.env(parent = parent.frame())
  if(!is.null(data)) {
    plyr::l_ply(
      seq_len(length(data)),
      function(i) assign(names(data)[i], data[[i]], envir = df_env)
    )
  }
  writeLines(text, con = 'output.r')
  knitr::spin('output.r', knit = T, envir = eval_env, precious = F, doc = '^##\\s*')
  ls(eval_env)
}

