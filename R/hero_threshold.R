
#' @export
run_hero_threshold <- function(...) {
    model <- list(...)
    
    # Handle progress reporting. Because the number of iterations for
    # threshold analysis can't be predicted in advance, we use a
    # separate progress reporting system that only runs once per model
    # run. Accordingly, we do not pass down a progress reporter to
    # the results runner for more granular progress like we normally
    # do.
    if (is.null(model$create_progress_reporter_factory)) {
        create_progress_reporter <- create_null_prog_reporter
        progress_reporter <- create_progress_reporter()
    } else {
        create_progress_reporter <- model$create_progress_reporter_factory()
        progress_reporter <- (model$create_progress_reporter_factory())()
    }
    model$create_progress_reporter_factory <- NULL
    max_prog <- get_threshold_max_progress(model)
    progress_reporter$report_max_progress(max_prog)

    check_threshold_analyses(model)
    progress_reporter$report_progress(5)

    n_analyses <- nrow(model$threshold_analyses)

    res_list <- list()
    for (i in seq_len(n_analyses)) {
        res_list[[i]] <- run_single_analysis(model, model$threshold_analyses[i, ], progress_reporter)
    }
    
    res <- aggregate_threshold_analysis_results(model$threshold_analyses, res_list)
    progress_reporter$report_progress(5)

    res
}

check_threshold_analyses <- function(model) {
    analyses <- model$threshold_analyses

    # Check that it is a data.frame and isn't empty
    if (is.null(analyses) || !("data.frame" %in% class(analyses)) || nrow(analyses) == 0) {
        stop(error_codes$threshold_no_analyses, call. = F)
    }

    # Check each analysis
    model$threshold_analyses %>%
        rowwise() %>%
        group_split() %>%
        walk(check_threshold_analysis, model)
}

isNumericOrInteger <- function(x) {
  is.numeric(x) | is.integer(x)
}

check_threshold_analysis <- function(analysis, model) {

    name <- analysis$name
    if (is.null(name)) {
        name <- ''
    }

    # Check that is has the right top-level fields
    required_fields <- c('name', 'param', 'range', 'condition')

    missing_fields_indicator <- !required_fields %in% colnames(analysis)
    if (any(missing_fields_indicator)) {
        missing_fields <- required_fields[missing_fields_indicator]
        missing_field_csl <- vector_to_cs_string(missing_fields, quoted = T)
        stop(glue(error_codes$threshold_missing_fields, name = name, fields = missing_field_csl), call. = F)
    }

    # Check that range is a list with upper and lower specified as numeric
    range_class <- class(analysis$range)
    bounds_numeric <- isNumericOrInteger(analysis$range$lower) & isNumericOrInteger(analysis$range$upper)
    bounds_undefined <- c(is.na(analysis$range$lower), is.na(analysis$range$upper))

    if (range_class != 'data.frame' || !bounds_numeric || any(bounds_undefined)) {
        stop(glue(error_codes$threshold_invalid_range, name = name), call. = F)
    }

    # Check that input variable exists
    param_missing <- !analysis$param %in% model$variables$name
    if (param_missing) {
        stop(glue(error_codes$threshold_input_not_found, name = name, param = analysis$param), call. = F)
    }

    # Check output
    invalid_output <- !(analysis$condition$output %in% c('outcomes', 'costs', 'nmb', 'ce', 'vbp'))
    if (invalid_output) {
        stop(glue(error_codes$threshold_invalid_output, name = name, output = analysis$condition$output), call. = F)
    }

    # Check outcome(s) exist(s)
    if (analysis$condition$output %in% c('nmb', 'ce')) {
        valid_econ_outcomes <- unique(model$esumm$name)
        if (!analysis$condition$econ_outcome %in% valid_econ_outcomes) {
            stop(
                glue(
                    error_codes$threshold_invalid_econ_outcome,
                    name = name,
                    outcome = analysis$condition$econ_outcome
                ),
                call. = F
            )
        }

        valid_health_outcomes <- unique(model$hsumm$name)
        if (!analysis$condition$health_outcome %in% valid_health_outcomes) {
            stop(
                glue(
                    error_codes$threshold_invalid_health_outcome,
                    name = name,
                    outcome = analysis$condition$health_outcome
                ),
                call. = F
            )
        }

    } else if (analysis$condition$output == 'outcomes') {

        valid_outcomes <- c(unique(model$hsumm$name), unique(model$hvalues$name))
        if (!analysis$condition$outcome %in% valid_outcomes) {
            stop(
                glue(
                    error_codes$threshold_invalid_health_outcome,
                    name = name,
                    outcome = analysis$condition$outcome
                ),
                call. = F
            )
        }

    } else if (analysis$condition$output == 'costs') {


        valid_outcomes <- c(unique(model$esumm$name), unique(model$evalues$name))
        if (!analysis$condition$outcome %in% valid_outcomes) {
            stop(
                glue(
                    error_codes$threshold_invalid_econ_outcome,
                    name = name,
                    outcome = analysis$condition$outcome
                ),
                call. = F
            )
        }

    }

    # Check condiiton fields aren't missing
    required_fields <- get_required_condition_fields(analysis$condition)
    condition_field_names <- colnames(analysis$condition)
    missing_condition_fields_indicator <- !required_fields %in% condition_field_names
    if (any(missing_condition_fields_indicator)) {
        missing_condition_fields <- required_fields[missing_condition_fields_indicator]
        missing_condition_field_csl <- vector_to_cs_string(missing_condition_fields, quoted = T)
        stop(glue(error_codes$threshold_condition_invalid, name = name, fields = missing_condition_field_csl), call. = F)
    }

}

get_required_condition_fields <- function(condition) {
    if (is.null(condition$output)) {
        required_fields <- c('output')
    } else if (condition$output %in% c('outcomes', 'costs')) {
        if (!is.null(condition$type) && condition$type == 'difference') {
            required_fields <- c('output', 'outcome', 'discount', 'type', 'referent', 'comparator', 'value')
        } else {
            required_fields <- c('output', 'outcome', 'discount', 'type', 'strategy', 'value')
        }
    } else if(condition$output == 'nmb') {
        required_fields <- c('output', 'health_outcome', 'econ_outcome', 'referent', 'comparator', 'value')
    } else if(condition$output == 'ce') {
        required_fields <- c('output', 'health_outcome', 'econ_outcome', 'referent', 'comparator')
    } else if (condition$output == 'vbp') {
        required_fields <- c('output', 'comparator', 'value')
    } else {
        required_fields <- c('output')
    }

    required_fields
}

run_single_analysis <- function(model, analysis, progress_reporter) {
    solver_instance <- create_solver_instance(model, analysis, progress_reporter)
    threshold_value <- find_threshold_value(solver_instance$run_iteration, analysis)
    solver_instance$complete_progress()
    list(
        name = analysis$name,
        param = analysis$param,
        threshold_value = threshold_value,
        history = solver_instance$get_history()
    )
}

aggregate_threshold_analysis_results <- function(analyses, results_list) {
    list(
        analyses = analyses,
        threshold_values = map_dfr(
            results_list,
            function(x) {
                tibble(
                    name = x$name,
                    param = x$param,
                    value = x$threshold_value
                )
            }
        ),
        root_finder_history = map_dfr(
            results_list,
            function(x) {
                relocate(mutate(x$history, name = x$name, param = x$param), name, param)
            }
        ),
        api_ver = '2.0'
    )
}

create_solver_instance <- function(model, analysis, progress_reporter) {
    history <- list()
    iteration <- 0
    max_progress_analysis <- 100
    cumulative_progress <- 0
    min_rel_progress <- 0.1

    list(
        run_iteration = function(x) {
            model <- set_solver_input(model, analysis, x)
            runner <- get_solver_results_runner(analysis)
            results <- do.call(runner, model)
            dataset <- get_solver_results_dataset(results, analysis)
            target_res <- get_solver_result(dataset, analysis)

            goal <- get_solver_goal(analysis)


            diff <- target_res - goal

            iteration <<- iteration + 1
            # Handle progress reporting
            remaining_progress <- max_progress_analysis - cumulative_progress
            progress_to_report <- remaining_progress * min_rel_progress
            # if (iteration > 1) {
            #     last_diff <- abs(history[[iteration - 1]]$diff)
            #     diff_in_diff <- last_diff - abs(diff)
            #     if (diff_in_diff > 0) {
            #         relative_progress <- diff_in_diff / last_diff
            #         progress_to_report <- max(
            #             relative_progress * remaining_progress,
            #             progress_to_report
            #         )
            #     }
            # }
            progress_reporter$report_progress(progress_to_report)
            cumulative_progress <<- cumulative_progress + progress_to_report

            history[[iteration]] <<- tibble(
                iteration = iteration,
                input = x,
                output = target_res,
                goal = goal,
                diff = diff
            )

            diff
        },
        get_history = function() bind_rows(history),
        complete_progress = function() {
            remaining_progress <- max_progress_analysis - cumulative_progress
            progress_reporter$report_progress(remaining_progress)
        }
    )
}

find_threshold_value <- function(solver_callback, analysis) {
    res <- tryCatch(
        threshold_root_finder(solver_callback, analysis),
        error = function(e) {
            if (grepl("f() values at end points not of opposite sign", e, fixed = TRUE)) {
                warning(glue(error_codes$warn_threshold_unit, name = analysis$name), call. = F)
                threshold_optimizer(solver_callback, analysis)
            } else {
                stop(e$message, call. = F)
            }
        }
    )
}

threshold_root_finder <- function(solver_callback, analysis) {
    res <- uniroot(solver_callback, c(analysis$range$lower, analysis$range$upper))

    res$root
}

threshold_optimizer <- function(solver_callback, analysis) {
    goal_func <- function(x) abs(solver_callback(x))
    tol <- .Machine$double.eps^0.25
    res <- optimize(
      goal_func,
        lower = analysis$range$lower,
        upper = analysis$range$upper,
        maximum = FALSE,
        tol = tol
    )

    # Return NA if acceptably close solution isn't found
    if (goal_func(res$minimum) > (tol * 2)) {
        return(NA)
    }

}

get_solver_results_dataset <- function(result, analysis) {
    if (analysis$condition$output == 'ce') {
        dataset <- result$nmb
    } else {
        dataset <- result[[analysis$condition$output]]
    }
    dataset
}

get_solver_results_runner <- function(analysis) {
    if (analysis$condition$output == 'vbp') {
        runner <- run_hero_vbp
    } else {
        runner <- run_hero_bc
    }
}

get_solver_result <- function(result, analysis) {

    getter <- switch(
        analysis$condition$output,
        'outcomes' = get_solver_result_outcomes_costs(analysis),
        'costs' =  get_solver_result_outcomes_costs(analysis),
        'nmb' =  get_solver_result_nmb(analysis),
        'ce' =  get_solver_result_nmb(analysis),
        'vbp' = get_solver_result_vbp(analysis)
    )
    
    getter(result)
}

get_solver_result_vbp <- function(analysis) {
    condition <- analysis$condition
    function(dataset) {

        if (condition$comparator == 'All') {
            res <- min(dataset$value)
        } else {
            res <- dataset$value[dataset$series == condition$comparator]
        }

        res
    }
}

get_solver_result_nmb <- function(analysis) {
    condition <- analysis$condition
    function(dataset) {

        isCorrectSeries <- dataset$series == paste0(condition$referent, ' vs. ', condition$comparator)

        isCorrectHealthOutcome <- dataset$outcome ==condition$health_outcome
        if (!any(isCorrectHealthOutcome)) {
            #error
        }
        isCorrectEconOutcome <- dataset$outcome ==condition$econ_outcome
        if (!any(isCorrectEconOutcome)) {
            #error
        }

        filteredResultsDataset <- dataset[isCorrectSeries & (isCorrectHealthOutcome | isCorrectEconOutcome), ]

        sum(filteredResultsDataset$value)
    }
}

get_solver_result_outcomes_costs <- function(analysis) {
    condition <- analysis$condition
    function(dataset) {

        if (condition$type == 'difference') {
            isCorrectSeries <- dataset$series == paste0(condition$referent, ' vs. ', condition$comparator)
        } else {
            isCorrectSeries <- dataset$series == condition$strategy
        }

        isSummary <- condition$outcome %in% dataset$outcome
        isValue <- condition$outcome %in% dataset$group

        if (isSummary) {
            isCorrectOutcome <- dataset$outcome == condition$outcome
        } else {
            if (isValue) {
                isCorrectOutcome <- dataset$group == condition$outcome
            } else {
                # error
            }
        }

        discIsCorrect <- dataset$disc == condition$discount

        filteredResultsDataset <- dataset[isCorrectSeries & isCorrectOutcome & discIsCorrect, ]

        sum(filteredResultsDataset$value)
    }
}

set_solver_input <- function(model, analysis, value) {

    target_var_index <- model$variables$name == analysis$param
    model$variables$value[target_var_index] <- value
    model
}

get_solver_goal <- function(analysis) {

    if (analysis$condition$output == 'ce') {
        goal <- 0
    } else {
        goal <- analysis$condition$value
    }

    goal
}
