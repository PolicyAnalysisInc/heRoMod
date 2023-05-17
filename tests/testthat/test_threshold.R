context("Test threshold analysis")

json_model <- jsonlite::read_json(
    system.file(
        "compat",
        "markov.json",
        package = "heRomod"
    ),
    simplifyVector = TRUE
)
model <- heRomod:::convert_model(json_model)
model$cores <- 1
test_that(
    "Threshold analysis checks inputs",
    {
        # NO ANALYSES DEFINED

        test_model1<- model
        test_model1$threshold_analyses <- NULL
        expect_error(
            do.call(run_hero_threshold, test_model1),
            "Error in threshold analysis, no analyses were defined."
        )

        test_model1<- model
        test_model1$threshold_analyses <- list()
        expect_error(
            do.call(run_hero_threshold, test_model1),
            "Error in threshold analysis, no analyses were defined."
        )

        test_model1<- model
        test_model1$threshold_analyses <- data.frame()
        expect_error(
            do.call(run_hero_threshold, test_model1),
            "Error in threshold analysis, no analyses were defined."
        )

        # MISSING KEYS AT TOP LEVEL

        test_model1<- model
        test_model1$threshold_analyses <- select(test_model1$threshold_analyses, -condition, -range)
        expect_error(
            do.call(run_hero_threshold, test_model1),
            'Error in threshold analysis "analysis -1", analysis missing required fields: "range", "condition".'
        )

        test_model1<- model
        test_model1$threshold_analyses <- select(test_model1$threshold_analyses, -condition)
        expect_error(
            do.call(run_hero_threshold, test_model1),
            'Error in threshold analysis "analysis -1", analysis missing required fields: "condition".'
        )

        # INPUT PARAMETER DOES NOT EXIST

        test_model1<- model
        test_model1$threshold_analyses$param[2] <- 'foo'
        expect_error(
            do.call(run_hero_threshold, test_model1),
            'Error in threshold analysis "analysis 0", input parameter "foo" not found in model.'
        )

        # OUTPUT VALUE DOES NOT EXIST

        test_model1<- model
        test_model1$threshold_analyses$condition$output[3] <- 'nmbb'
        expect_error(
            do.call(run_hero_threshold, test_model1),
            'Error in threshold analysis "analysis 1", output "nmbb" is not valid.'
        )

        # HEALTH OUTCOME DOES NOT EXIST

        test_model1<- model
        test_model1$threshold_analyses$condition$health_outcome[3] <- 'qalllys'
        expect_error(
            do.call(run_hero_threshold, test_model1),
            'Error in threshold analysis "analysis 1", health outcome "qalllys" is not valid.'
        )

        test_model1<- model
        test_model1$threshold_analyses$condition$outcome[6] <- 'qalllys'
        expect_error(
            do.call(run_hero_threshold, test_model1),
            'Error in threshold analysis "analysis 4", health outcome "qalllys" is not valid.'
        )

        # ECON OUTCOME DOES NOT EXIST

        test_model1<- model
        test_model1$threshold_analyses$condition$outcome[5] <- 'costss'
        expect_error(
            do.call(run_hero_threshold, test_model1),
            'Error in threshold analysis "analysis 3", economic outcome "costss" is not valid.'
        )

        test_model1<- model
        test_model1$threshold_analyses$condition$econ_outcome[3] <- 'costss'
        expect_error(
            do.call(run_hero_threshold, test_model1),
            'Error in threshold analysis "analysis 1", economic outcome "costss" is not valid.'
        )


    }
)

test_that(
    "Threshold analysis produces correct result",
    {
        local_edition(3)
        test_model1<- model
        expect_warning(
            suppressMessages(res <- do.call(run_hero_threshold, test_model1)),
            'Warning in threshold analysis "analysis 5", could not be completed using uniroot. Using optimize instead.'
        )
        expect_snapshot_value(res, style = 'serialize', cran = TRUE)
    }
)

test_that(
  "Threshold analysis only runs active analyses",
  {
    local_edition(3)
    test_model1<- model
    test_model1$threshold_analyses$active <- c('On', 'On', 'Off', 'Off', 'Off', 'Off', 'Off')
    suppressMessages(res <- do.call(run_hero_threshold, test_model1))
    expect_equal(2, nrow(res$threshold_values))
    expect_snapshot_value(res, style = 'serialize', cran = TRUE)
  }
)

test_that(
    "Throws with normal error if model won't run",
    {
        test_model1<- model
        test_model1$variables$value[1] <- 'stop("foo")'
        expect_error(
            suppressMessages(res <- do.call(run_hero_threshold, test_model1)),
            "Error in parameter 'p_sick_nat', foo"
        )
    }
)