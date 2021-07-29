convert_model <- function(model) {
    converted_strats <- convert_strategies(model$strategieshow)
    list(
        decision = 'how', # Not used, but here to pass tests
        settings = convert_settings(model$settings, model$settingsOverrides),
        groups = convert_groups(model$groups),
        strategies = converted_strats,
        states = convert_states(model$states),
        transitions = convert_transitions(get_transitions(model), converted_strats, model$modelheader, model$settings),
        hvalues = convert_hvalues(model$values, converted_strats),
        evalues = convert_evalues(model$values, converted_strats),
        hsumms = convert_hsumms(model$summaries),
        esumms = convert_esumms(model$summaries),
        variables = convert_variables(model$formulas),
        tables = convert_tables(model$tables),
        scripts = convert_scripts(model$scripts),
        surv_dists = convert_surv_dists(model$surv_dists),
        type = NULL,
        vbp = model$vbp,
        psa = convert_psa(model$psa, model$psa_correlations),
        dsa_settings = model$dsa_settings,
        scenario_settings = model$scenario_settings,
        scenario = convert_scenarios(model$scenarios),
        cores = get_n_cores(model$cores),
        script_to_run = model$script_to_run,
        report_max_progress = model$report_max_progress,
        report_progress = model$report_progress,
        .manifest = model$.manifest,
        name = safe_filename(model$modelheader$filename)
    )
}

run_code_preview_compat <- function(...) {
    data <- list(...)
    do.call(
        run_markdown,
        list(
            text = data$scripts[[data$script_to_run]],
            data = data$tables,
            report_max_progress = data$report_max_progress,
            report_progress = data$report_progress,
            .manifest = data$.manifest,
            name = data$name
        )
    )
}

get_n_cores <- function(cores) {
    if (is.null(cores)) return(as.numeric(system('nproc',intern = T)))
    cores
}

convert_settings <- function(settings, overrides) {

    # Apply overrides to settings
    merged_settings <- list_modify(settings, !!!overrides)

    # Return settings object
    settings_list <- list(
        disc_cost = get_cost_dr(merged_settings),
        disc_eff = get_outcomes_dr(merged_settings),
        n_cycles = get_n_cycles(merged_settings),
        method = get_hc_method(merged_settings),
        disc_method = get_disc_method(merged_settings),
        CycleLength = get_cycle_length(merged_settings),
        CycleLengthUnits = get_cycle_length_units(merged_settings),
        ModelTimeframe = get_timeframe(merged_settings),
        ModelTimeframeUnits = get_timeframe_units(merged_settings),
        days_per_year = get_days_per_year(merged_settings)
    )
    
    discard(settings_list, is.null)
}

convert_groups <- function(groups) {
    if (is.null(groups) || class(groups) == "list") {
        return(groups)
    }
    groups %>%
        filter(on_off == "On") %>%
        mutate(name = paste0('"', name, '"')) %>%
        select(-label, -id, -on_off)
}

convert_states <- function(states) {
    transmute(states,name = name, desc = label, prob = initial_probability, limit = limit)
}

convert_strategies <- function(strategies) {
    strategies %>%
        filter(on_off == "On") %>%
        transmute(name = name, desc = label)
}

convert_transitions <- function(transitions, strategies, header, settings) {
    switch(
        header$modelType,
        "Markov" = convert_markov_transitions(transitions, strategies),
        "PS" = convert_psm_transitions(transitions, strategies, settings),
        "PSCustom" = convert_custom_transitions(transitions, strategies)
    )
}

get_transitions <- function(model) {
    switch(
        model$modelheader$modelType,
        "Markov" = model$transitions,
        "PS" = model$psm_transitions,
        "PSCustom" = model$transitions
    )
}

convert_markov_transitions <- function(transitions, strategies) {
    transitions %>%
        filter(strategy %in% c("All", strategies$name)) %>%
        transmute(strategy, from, to, value = formula)
}

convert_psm_transitions <- function(transitions, strategies, settings) {
    dpy <- 365
    if (!is.null(settings$days_per_year)) {
        dpy <- settings$days_per_year
    }
    filtered_transitions <- filter(transitions, strategy %in% c("All", strategies$name))
    cycle_length <- get_cycle_length(settings)
    cycle_length_units <- get_cycle_length_units(settings)
    cycle_length_days <- time_in_days(cycle_length_units, dpy) * cycle_length
    surv_cycle_unit_days <- map_dbl(filtered_transitions$unit, function(x) time_in_days(x, dpy))
    surv_cycle_length_days <- cycle_length_days / surv_cycle_unit_days
    transmute(
        filtered_transitions,
        strategy = strategy,
        endpoint = endpoint,
        cycle_length = surv_cycle_length_days,
        value = formula
    )
}

convert_custom_transitions <- function(transitions, strategies) {
    transitions %>%
        filter(strategy %in% c("All", strategies$name)) %>%
        transmute(strategy, state, value = formula)
}

convert_hvalues <- function(values, strategies) {
    values %>%
        filter(category == "Health", strategy %in% c("All", strategies$name)) %>%
        transmute(
            name,
            label = description,
            strategy,
            state = map2_chr(state, type, function(x, y) if(y == "Transition") paste0(x$from, "\u2192", x$to) else x),
            value = formula
        )
}

convert_evalues <- function(values, strategies) {
    values %>%
        filter(category == "Economic", strategy %in% c("All", strategies$name)) %>%
        transmute(
            name,
            label = description,
            strategy,
            state = map2_chr(state, type, function(x, y) if(y == "Transition") paste0(x$from, "\u2192", x$to) else x),
            value = formula
        )
}

convert_hsumms <- function(summaries) {
    summaries %>%
        filter(category == "Health") %>%
        rowwise() %>%
        group_split() %>%
        map(function(summary) {
            data.frame(
                name = summary$name,
                description = summary$description,
                value = summary$values[[1]],
                wtp = as.numeric(summary$wtp),
                stringsAsFactors = F
            )
        }) %>%
        bind_rows()
}

convert_esumms <- function(summaries) {
    summaries %>%
        filter(category == "Economic") %>%
        rowwise() %>%
        group_split() %>%
        map(function(summary) {
            data.frame(
                name = summary$name,
                description = summary$description,
                value = summary$values[[1]],
                wtp = as.numeric(NA),
                stringsAsFactors = F
            )
        }) %>%
        bind_rows()
}

convert_variables <- function(variables) {
    print('hi')
    transmute(
        variables,
        name = name,
        desc = description,
        value = as.character(ifelse(!is.na(overrideActive) & overrideActive == "On", overrideValue, formula)),
        low = as.character(ifelse(!is.na(active) & active == "On", low, "")),
        high = as.character(ifelse(!is.na(active) & active == "On", high, "")),
        psa = as.character(ifelse(!is.na(psa_active) & psa_active == "On", distribution, ""))
    )
}

convert_surv_dists <- function(surv_dists) {
    if (is.null(surv_dists) || class(surv_dists) == "list") {
        return(surv_dists)
    }
    transmute(
        surv_dists,
        name = name,
        value = as.character(formula)
    )
}

convert_scripts <- function(scripts) {
    if (is.null(scripts) || class(scripts) == "list") {
        return(structure(list(), names=character(0)))
    }
    scripts %>%
        rowwise() %>%
        group_split() %>%
        map(function(script) script$text) %>%
        set_names(scripts$name)
}

convert_tables <- function(tables) {
    if (is.null(tables) || class(tables) == "list") {
        return(structure(list(), names=character(0)))
    }
    names <- tables$name
    tables$data %>%
        map(function(mat) {
            tbl <- mat %>%
                as.data.frame(stringsAsFactors = F) %>%
                select_if(function(x) any(x != '' & x != 0 & !is.na(x)))
            colnames <- as.character(tbl[1, ])
            data <- tbl[-1, ] %>%
                filter_all(any_vars(. != '')) %>%
                mutate_all(function(x) {
                    number <- suppressWarnings(as.numeric(x))
                    if (!any(is.na(number))) {
                        return(number)
                    }
                    x
                }) %>%
                set_names(colnames)
        }) %>%
        set_names(names)
}

convert_psa <- function(psa, correlations) {
    var_names <- correlations$variables
    correlations <- correlations$data
    if (is.null(correlations) || class(correlations) == "list") {
        psa$correlation <- list()
    } else {
        n_var <- length(var_names)
        n_correls <- (n_var^2 - n_var)/2
        correls <- data.frame(
            var1 = character(n_correls),
            var2 = character(n_correls),
            value = numeric(n_correls),
            stringsAsFactors = F
        )
        row_index <- 1
        col_index <- 1
        for (i in seq_len(n_correls)) {
            correls$var1[i] <- var_names[row_index + 1]
            correls$var2[i] <- var_names[col_index]
            correls$value[i] <- as.numeric(correlations[row_index + 1, col_index])
            col_index <- col_index + 1
            if (col_index > row_index) {
                row_index <- row_index + 1
                col_index <- 1
            }
        }
        psa$correlation <- correls
    }
    psa$parallel <- T
    #psa$n <- 50L
    psa
}

convert_scenarios <- function(scenarios) {
    if (is.null(scenarios) || class(scenarios) == "list") {
        return(list())
    }
    scenarios %>%
        filter(active) %>%
        rowwise() %>%
        group_split() %>%
        map(function(x) {
            transmute(
                x$params[[1]],
                scenario_name = x$name,
                description = x$description,
                param_name = x$params[[1]]$name,
                formula = x$params[[1]]$scen_value
            )
        }) %>%
        bind_rows()
}

# Settings Getter Functions
get_cost_dr <- function(settings) {
    as.numeric(settings$DiscountRateOutcomes) / 100
}
get_outcomes_dr <- function(settings) {
    as.numeric(settings$DiscountRateCosts) / 100
}
get_n_cycles <- function(settings) {
    as.integer(settings$CycleCount)
}
get_hc_method <- function(settings) {
    if (is.null(settings$method)) {
        return("life-table")
    }
    switch(
        settings$method,
        "start" = "beginning",
        "end" = "end",
        "life-table" = "life-table"
    )
}
get_disc_method <- function(settings) {
    if (is.null(settings$disc_method)) {
        return("start")
    }
    settings$discMethod
}
get_cycle_length <- function(settings) {
    settings$CycleLength
}
get_cycle_length_units <- function(settings) {
    settings$CycleLengthUnits
}
get_timeframe <- function(settings) {
    settings$ModelTimeframe
}
get_timeframe_units <- function(settings) {
    settings$ModelTimeframeUnits
}
get_days_per_year <- function(settings) {
    settings$days_per_year
}