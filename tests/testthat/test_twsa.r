context("2WSA")

model <- readRDS(system.file("hero", "groups", "model.rds", package="heRomod"))
model_no_groups <- readRDS(system.file("hero", "markov_model", "model.rds", package="heRomod"))

test_that("Missing specifications are detected", {
  
    # TWSA Parameters are missing
    model1 <- model
    model1$twsa_parameters <- NULL
    expect_error(
      do.call(run_hero_twsa, model1),
      'Two-way sensitivity analysis must be configured before running.'
    )
    
    # TWSA Parameters is empty
    model2 <- model
    model2$twsa_parameters <- model2$twsa_parameters[c(), ]
    expect_error(
      do.call(run_hero_twsa, model2),
      'Two-way sensitivity analysis must be configured before running.'
    )
    
    # TWSA is missing
    model3 <- model
    model3$twsa <- NULL
    expect_error(
      do.call(run_hero_twsa, model3),
      'Two-way sensitivity analysis must be configured before running.'
    )
    
    # TWSA is empty
    model4 <- model
    model4$twsa <- model4$twsa[c(), ]
    expect_error(
      do.call(run_hero_twsa, model4),
      'Two-way sensitivity analysis must be configured before running.'
    )
    
})

test_that("Invalid specifications are detected", {
  
  # Wrong columns for TWSA
  model1 <- model
  model1$twsa <- model1$twsa[, 1]
  expect_error(
    do.call(run_hero_twsa, model1),
    'Invalid format for two-way sensitivity analysis.'
  )
  
  # Nonexistent parameter
  model2 <- model
  model2$twsa_parameters$name[1] <- 'foo'
  expect_error(
    do.call(run_hero_twsa, model2),
    'Two-way sensitivity analysis references missing parameters: "foo".'
  )
  
  # Orphaned parameters
  model3 <- model
  model3$twsa <- model3$twsa[1, ]
  expect_error(
    do.call(run_hero_twsa, model3),
    'Two-way sensitivity analysis includes parameters that are not part of any analysis: "target_hr", "immun_hr".'
  )
  
  # Wrong number of parameters in a twsa
  model4 <- model
  model4$twsa_parameters <- model4$twsa_parameters[-1, ]
  expect_error(
    do.call(run_hero_twsa, model4),
    'Two-way sensitivity analyses must include exactly two parameters per analysis.'
  )
  
  # Bad type opition
  model5 <- model
  model5$twsa_parameters$type[1] <- 'foo'
  expect_error(
    do.call(run_hero_twsa, model5),
    'Parameters in two-way sensitivity analyses configured with invalid method for specifying values: "rfs_p1".'
  )
  
})

test_that("Syntax errors are reported properly", {
  
  model1 <- model
  model1$twsa_parameters$data$custom[3] <- '1+,2,3'
  expect_error(
    do.call(run_hero_twsa, model1),
    'Syntax error in parameter target_hr in two-way sensitivity analysis.'
  )
  
})

# test_that("Progress calculations are correct", {
# 
#   # Setup
#   model1 <- model
#   model1$states$limit <- 1
#   model1$cores <- 1
#   model1$twsa$active[1] <- F
#   model1$settings$ModelTimeframe <- 2
#   
#   model2 <- model_no_groups
#   model2$cores <- 1
#   model2$twsa$active[1] <- F
#   model2$settings$ModelTimeframe <- 2
#   
#   # Test 1
#   gen_progress_reporters <- function() {
#     max_progress <- 0
#     progress <- 0
#     list(
#       report_max_progress = function(x) {
#         max_progress <<- x
#       },
#       report_progress = function(x) {
#         progress <<- progress + x
#       },
#       get_max_progress = function() max_progress,
#       get_progress = function() progress
#     )
#   }
#   progress_reporters <- gen_progress_reporters()
#   model1$report_max_progress <- progress_reporters$report_max_progress
#   model1$report_progress <- progress_reporters$report_progress
#   res <- do.call(run_hero_twsa, model1)
#   expect_equal(progress_reporters$get_max_progress(), progress_reporters$get_progress())
#   
#   # Test 2
#   model1$twsa_settings$run_vbp <- F
#   gen_progress_reporters <- function() {
#     max_progress <- 0
#     progress <- 0
#     list(
#       report_max_progress = function(x) {
#         max_progress <<- x
#       },
#       report_progress = function(x) {
#         progress <<- progress + x
#       },
#       get_max_progress = function() max_progress,
#       get_progress = function() progress
#     )
#   }
#   progress_reporters <- gen_progress_reporters()
#   model1$report_max_progress <- progress_reporters$report_max_progress
#   model1$report_progress <- progress_reporters$report_progress
#   res <- do.call(run_hero_twsa, model1)
#   expect_equal(progress_reporters$get_max_progress(), progress_reporters$get_progress())
#   
#   # Test 3
#   model2 <- model_no_groups
#   model2$twsa_settings$run_vbp <- F
#   model2$cores <- 1
#   gen_progress_reporters <- function() {
#     max_progress <- 0
#     progress <- 0
#     list(
#       report_max_progress = function(x) {
#         max_progress <<- x
#       },
#       report_progress = function(x) {
#         progress <<- progress + x
#       },
#       get_max_progress = function() max_progress,
#       get_progress = function() progress
#     )
#   }
#   progress_reporters <- gen_progress_reporters()
#   model2$report_max_progress <- progress_reporters$report_max_progress
#   model2$report_progress <- progress_reporters$report_progress
#   res <- do.call(run_hero_twsa, model2)
#   expect_equal(progress_reporters$get_max_progress(), progress_reporters$get_progress())
# 
# })

test_that("Group or strategy-dependent parameters can't be varied", {

  model1 <- model
  model1$variables$value[7] <- "by_group(adults = 1.12, children = 1.2)"
  model1$cores <- 1
  expect_error(
    do.call(run_hero_twsa, model1),
    'Two-way sensitivity analysis may not vary parameters that are group or strategy dependent.'
  )

})